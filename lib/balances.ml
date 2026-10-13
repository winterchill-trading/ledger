open! Core
module Amount = Money.Amount
module Currency = Money.Currency

module Hold_state = struct
  type t =
    { id : Hold_id.t
    ; account : Account_id.t
    ; placed : Amount.t
    ; remaining : Amount.t
    ; placed_seq : int
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { accounts : Account.t Account_id.Map.t
  ; (* Debits minus credits, per account (pending sub-accounts are their own keys) and
       currency. *)
    signed : Amount.t Currency.Map.t Account_id.Map.t
  ; holds : Hold_state.t Hold_id.Map.t
  ; keys : int Idempotency_key.Map.t
  ; entries : Entry.t Int.Map.t
  ; reversals : int Int.Map.t
  ; next_seq : int
  }

let empty =
  { accounts = Account_id.Map.empty
  ; signed = Account_id.Map.empty
  ; holds = Hold_id.Map.empty
  ; keys = Idempotency_key.Map.empty
  ; entries = Int.Map.empty
  ; reversals = Int.Map.empty
  ; next_seq = 1
  }
;;

let next_seq t = t.next_seq
let accounts t = Map.data t.accounts

let find_account t id =
  match Map.find t.accounts id with
  | Some _ as found -> found
  | None ->
    Option.bind (Account_id.parent_of_pending id) ~f:(fun parent ->
      Option.map (Map.find t.accounts parent) ~f:(fun (account : Account.t) ->
        { account with id; allow_negative = false }))
;;

let signed_balance t id currency =
  match Option.bind (Map.find t.signed id) ~f:(fun m -> Map.find m currency) with
  | Some a -> a
  | None -> Amount.zero currency
;;

(* Debits-minus-credits seen from the account's normal side. *)
let normal (kind : Account_kind.t) signed =
  match Account_kind.normal_side kind with
  | Debit -> signed
  | Credit -> Amount.neg signed
;;

let balance t id currency =
  match find_account t id with
  | None -> Amount.zero currency
  | Some account -> normal account.kind (signed_balance t id currency)
;;

let available = balance
let held t id currency = balance t (Account_id.pending id) currency
let posted t id currency = Amount.add_exn (available t id currency) (held t id currency)

let debits_minus_credits t =
  Map.to_alist t.signed
  |> List.concat_map ~f:(fun (id, by_currency) ->
    Map.to_alist by_currency |> List.map ~f:(fun (currency, a) -> id, currency, a))
;;

let find_hold t id = Map.find t.holds id

let open_holds t =
  Map.data t.holds
  |> List.filter ~f:(fun (h : Hold_state.t) -> Amount.is_positive h.remaining)
;;

let find_entry t ~seq = Map.find t.entries seq
let find_idempotency_key t key = Map.find t.keys key
let reversed_by t ~seq = Map.find t.reversals seq

(* Validation *)

let fail = Or_error.error_s

let open_account t (account : Account.t) =
  let id = Account_id.to_string account.id in
  if Account_id.is_pending account.id
  then fail [%message "account ids ending in /pending are reserved" id]
  else if String.length id > Account_id.max_parent_length
  then
    fail
      [%message
        "account id too long for a pending sub-account"
          id
          ~max_length:(Account_id.max_parent_length : int)]
  else if Map.mem t.accounts account.id
  then fail [%message "account already open" id]
  else Ok { t with accounts = Map.set t.accounts ~key:account.id ~data:account }
;;

let amount_le a b = Amount.compare_value a b |> Or_error.ok_exn <= 0

(* The checks that depend on the entry's kind. Returns the new hold table and reversal
   links. *)
let check_kind t ~seq (entry : Entry.t) =
  let postings = entry.postings in
  let touches_pending =
    List.find postings ~f:(fun (p : Posting.t) -> Account_id.is_pending p.account)
  in
  let account_exn id = Option.value_exn (find_account t id) in
  (* For a hold on [account], the posting that moves [amount] out of (or back into) its
     pending sub-account. *)
  let hold_lookup id =
    match Map.find t.holds id with
    | None -> fail [%message "unknown hold" (id : Hold_id.t)]
    | Some h when not (Amount.is_positive h.remaining) ->
      fail [%message "hold is already closed" (id : Hold_id.t)]
    | Some h
      when not (Currency.equal (Amount.currency h.remaining) (Entry.currency entry)) ->
      fail [%message "entry currency differs from the hold's" (id : Hold_id.t)]
    | Some h -> Ok h
  in
  let reduce (h : Hold_state.t) amount =
    if not (amount_le amount h.remaining)
    then
      fail
        [%message
          "more than the hold's remaining amount"
            ~hold:(h.id : Hold_id.t)
            ~requested:(Amount.to_string amount)
            ~remaining:(Amount.to_string h.remaining)]
    else
      Ok
        ( Map.set
            t.holds
            ~key:h.id
            ~data:{ h with remaining = Amount.sub_exn h.remaining amount }
        , t.reversals )
  in
  match entry.kind with
  | (Transfer _ | Reversal _) when Option.is_some touches_pending ->
    fail
      [%message
        "only hold entries may post to a pending sub-account"
          (touches_pending : Posting.t option)]
  | Transfer _ -> Ok (t.holds, t.reversals)
  | Reversal { reverses } ->
    (match Map.find t.entries reverses with
     | None -> fail [%message "reversal of an unknown entry" (reverses : int)]
     | Some original ->
       (match original.kind, Map.find t.reversals reverses with
        | _, Some by ->
          fail [%message "entry already reversed" (reverses : int) (by : int)]
        | (Hold_place _ | Hold_release _ | Hold_capture _ | Reversal _), None ->
          fail
            [%message
              "only transfers can be reversed; release or capture a hold instead"
                (reverses : int)
                (original.kind : Entry.Kind.t)]
        | Transfer _, None ->
          let sorted l = List.sort l ~compare:Posting.compare in
          if [%equal: Posting.t list]
               (sorted postings)
               (sorted (List.map original.postings ~f:Posting.flip))
          then Ok (t.holds, Map.set t.reversals ~key:reverses ~data:seq)
          else
            fail
              [%message
                "a reversal must post the original's postings on the opposite sides"
                  (reverses : int)]))
  | Hold_place id ->
    if Map.mem t.holds id
    then fail [%message "hold id already used" (id : Hold_id.t)]
    else (
      match postings with
      | [ a; b ] ->
        let main, pending = if Account_id.is_pending a.account then b, a else a, b in
        let kind = (account_exn main.account).kind in
        let release_side = Account_kind.normal_side kind in
        if Account_id.equal pending.account (Account_id.pending main.account)
           && Side.equal pending.side release_side
           && Side.equal main.side (Side.flip release_side)
        then
          Ok
            ( Map.set
                t.holds
                ~key:id
                ~data:
                  { id
                  ; account = main.account
                  ; placed = main.amount
                  ; remaining = main.amount
                  ; placed_seq = seq
                  }
            , t.reversals )
        else
          fail
            [%message
              "a hold moves funds from an account to its own pending sub-account"
                (id : Hold_id.t)]
      | _ -> fail [%message "a hold entry has exactly two postings" (id : Hold_id.t)])
  | Hold_release id ->
    let%bind.Or_error h = hold_lookup id in
    let kind = (account_exn h.account).kind in
    let normal_side = Account_kind.normal_side kind in
    (match postings with
     | [ a; b ] ->
       let main, pending = if Account_id.is_pending a.account then b, a else a, b in
       if Account_id.equal main.account h.account
          && Account_id.equal pending.account (Account_id.pending h.account)
          && Side.equal main.side normal_side
          && Side.equal pending.side (Side.flip normal_side)
       then reduce h pending.amount
       else
         fail
           [%message
             "a release moves funds from the pending sub-account back to the account"
               (id : Hold_id.t)]
     | _ -> fail [%message "a release entry has exactly two postings" (id : Hold_id.t)])
  | Hold_capture id ->
    let%bind.Or_error h = hold_lookup id in
    let kind = (account_exn h.account).kind in
    let out_side = Side.flip (Account_kind.normal_side kind) in
    let pending_id = Account_id.pending h.account in
    let on_pending, others =
      List.partition_tf postings ~f:(fun (p : Posting.t) ->
        Account_id.equal p.account pending_id)
    in
    (match on_pending with
     | [ p ]
       when Side.equal p.side out_side
            && not
                 (List.exists others ~f:(fun (o : Posting.t) ->
                    Account_id.equal o.account h.account
                    || Account_id.is_pending o.account)) -> reduce h p.amount
     | _ ->
       fail
         [%message
           "a capture moves funds out of the hold's pending sub-account to other accounts"
             (id : Hold_id.t)])
;;

let post t ~seq (entry : Entry.t) =
  let open Or_error.Let_syntax in
  let%bind () =
    match Map.find t.keys entry.idempotency_key with
    | Some booked_at ->
      fail
        [%message
          "idempotency key already booked"
            ~idempotency_key:(entry.idempotency_key : Idempotency_key.t)
            (booked_at : int)]
    | None -> Ok ()
  in
  let%bind () =
    match
      List.find entry.postings ~f:(fun p -> Option.is_none (find_account t p.account))
    with
    | Some p ->
      fail
        [%message
          "posting to an account that is not open" ~account:(p.account : Account_id.t)]
    | None -> Ok ()
  in
  let%bind holds, reversals = check_kind t ~seq entry in
  let currency = Entry.currency entry in
  let signed =
    List.fold entry.postings ~init:t.signed ~f:(fun signed p ->
      Map.update signed p.account ~f:(fun by_currency ->
        let by_currency = Option.value by_currency ~default:Currency.Map.empty in
        Map.update by_currency currency ~f:(fun prev ->
          Amount.add_exn
            (Option.value prev ~default:(Amount.zero currency))
            (Posting.signed p))))
  in
  let t' =
    { t with
      signed
    ; holds
    ; reversals
    ; keys = Map.set t.keys ~key:entry.idempotency_key ~data:seq
    ; entries = Map.set t.entries ~key:seq ~data:entry
    }
  in
  let overdrawn =
    List.dedup_and_sort
      (List.map entry.postings ~f:(fun p -> p.account))
      ~compare:Account_id.compare
    |> List.find ~f:(fun id ->
      let account = Option.value_exn (find_account t' id) in
      (not account.allow_negative) && Amount.is_negative (balance t' id currency))
  in
  match overdrawn with
  | None -> Ok t'
  | Some id ->
    fail
      [%message
        "entry would take an account below zero"
          (id : Account_id.t)
          ~balance_after:(Amount.to_string (balance t' id currency))
          ~idempotency_key:(entry.idempotency_key : Idempotency_key.t)]
;;

let apply t ({ seq; event } : Record.t) =
  let open Or_error.Let_syntax in
  if seq <> t.next_seq
  then fail [%message "out-of-sequence record" (seq : int) ~expected:(t.next_seq : int)]
  else (
    let%map t =
      match event with
      | Account_opened account -> open_account t account
      | Posted entry -> post t ~seq entry
    in
    { t with next_seq = seq + 1 })
;;

let of_records records = List.fold_result records ~init:empty ~f:apply
