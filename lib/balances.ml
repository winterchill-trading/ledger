open! Core
module Amount = Money.Amount
module Currency = Money.Currency

module Hold_state = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : Hold_id.Stable.V1.t
        ; account : Account_id.Stable.V1.t
        ; placed : Amount.Stable.V1.t
        ; remaining : Amount.Stable.V1.t
        ; placed_by : Idempotency_key.Stable.V1.t
        ; expires_at : Time_ns.Stable.Alternate_sexp.V1.t option
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Columns = struct
  type t =
    { debits_posted : Amount.t
    ; credits_posted : Amount.t
    ; debits_pending : Amount.t
    ; credits_pending : Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Day_row = struct
  module Stable = struct
    module V1 = struct
      type t =
        { account : Account_id.Stable.V1.t
        ; currency : Currency.Stable.V1.t
        ; opening_booked : Amount.Stable.V1.t
        ; debits : Amount.Stable.V1.t
        ; credits : Amount.Stable.V1.t
        ; closing_booked : Amount.Stable.V1.t
        ; closing_available : Amount.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

let proposal_lifetime = Time_ns.Span.of_hr 24.

module Proposal = struct
  module Status = struct
    module Stable = struct
      module V1 = struct
        type t =
          | Proposed
          | Approved
          | Rejected
        [@@deriving bin_io, sexp, compare, equal]
      end
    end

    include Stable.V1
  end

  module Stable = struct
    module V1 = struct
      type t =
        { entry : Entry.Stable.V1.t
        ; maker : string
        ; reason : string
        ; ticket : string
        ; proposed_at : Time_ns.Stable.Alternate_sexp.V1.t
        ; status : Status.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let is_expired t ~now = Time_ns.( >= ) now (Time_ns.add t.proposed_at proposal_lifetime)
end

(* A pair of cumulative amounts in one currency: debits and credits of an account, or
   (placed, released) hold flows between an account and its pending sub-account. *)
type pair =
  { a : Amount.t
  ; b : Amount.t
  }

let zero_pair currency = { a = Amount.zero currency; b = Amount.zero currency }

module Key_state = struct
  type t =
    | Booked of
        { seq : int
        ; entry : Entry.t
        }
    | Rejected of
        { seq : int
        ; reason : string
        }
end

type by_account = pair Currency.Map.t Account_id.Map.t

type t =
  { accounts : Account.t Account_id.Map.t
  ; statuses : Account_status.t Account_id.Map.t
  ; (* debits (a) and credits (b), per account (pending sub-accounts are their own keys) *)
    totals : by_account
  ; (* per account: placed (a) into and released (b) back from its pending sub-account *)
    internal : by_account
  ; (* per account: debits_posted (a) and credits_posted (b) at the last close *)
    at_close : by_account
  ; holds : Hold_state.t Hold_id.Map.t
  ; proposals : Proposal.t Idempotency_key.Map.t
  ; keys : Key_state.t Idempotency_key.Map.t
  ; reversals : Idempotency_key.t Idempotency_key.Map.t
  ; next_seq : int
  ; last_recorded_at : Time_ns.t option
  ; last_business_date : Date.t option
  ; closed_through : Date.t option
  ; last_close_rows : Day_row.t list
  }

let empty =
  { accounts = Account_id.Map.empty
  ; statuses = Account_id.Map.empty
  ; totals = Account_id.Map.empty
  ; internal = Account_id.Map.empty
  ; at_close = Account_id.Map.empty
  ; holds = Hold_id.Map.empty
  ; proposals = Idempotency_key.Map.empty
  ; keys = Idempotency_key.Map.empty
  ; reversals = Idempotency_key.Map.empty
  ; next_seq = 1
  ; last_recorded_at = None
  ; last_business_date = None
  ; closed_through = None
  ; last_close_rows = []
  }
;;

let next_seq t = t.next_seq
let last_recorded_at t = t.last_recorded_at
let closed_through t = t.closed_through
let last_close_rows t = t.last_close_rows
let accounts t = Map.data t.accounts

let find_account t id =
  match Map.find t.accounts id with
  | Some _ as found -> found
  | None ->
    Option.bind (Account_id.parent_of_pending id) ~f:(fun parent ->
      Option.map (Map.find t.accounts parent) ~f:(fun (account : Account.t) ->
        { account with id; allow_negative = false }))
;;

let main_account id = Option.value (Account_id.parent_of_pending id) ~default:id

let status t id =
  Map.find t.statuses (main_account id) |> Option.value ~default:Account_status.Open
;;

let get (m : by_account) id currency =
  match Option.bind (Map.find m id) ~f:(fun m -> Map.find m currency) with
  | Some p -> p
  | None -> zero_pair currency
;;

let bump (m : by_account) id currency ~a ~b =
  Map.update m id ~f:(fun by_currency ->
    let by_currency = Option.value by_currency ~default:Currency.Map.empty in
    Map.update by_currency currency ~f:(fun prev ->
      let p = Option.value prev ~default:(zero_pair currency) in
      { a = Amount.add_exn p.a a; b = Amount.add_exn p.b b }))
;;

let signed_balance t id currency =
  let p = get t.totals id currency in
  Amount.sub_exn p.a p.b
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

let columns t id currency : Columns.t =
  let id = main_account id in
  let kind =
    Option.value_map (find_account t id) ~default:Account_kind.Liability ~f:Account.kind
  in
  let main = get t.totals id currency in
  let pending = get t.totals (Account_id.pending id) currency in
  let flows = get t.internal id currency in
  let placed = flows.a
  and released = flows.b in
  let ( + ) = Amount.add_exn
  and ( - ) = Amount.sub_exn in
  let zero = Amount.zero currency in
  (* See the .mli: moves between the account and its own pending sub-account are not
     posted. [out] is the side a payment leaves the account on. *)
  match Account_kind.normal_side kind with
  | Credit ->
    { debits_posted = main.a - placed + (pending.a - released)
    ; credits_posted = main.b - released + (pending.b - placed)
    ; debits_pending = held t id currency
    ; credits_pending = zero
    }
  | Debit ->
    { credits_posted = main.b - placed + (pending.b - released)
    ; debits_posted = main.a - released + (pending.a - placed)
    ; credits_pending = held t id currency
    ; debits_pending = zero
    }
;;

let debits_minus_credits t =
  Map.to_alist t.totals
  |> List.concat_map ~f:(fun (id, by_currency) ->
    Map.to_alist by_currency
    |> List.map ~f:(fun (currency, p) -> id, currency, Amount.sub_exn p.a p.b))
;;

let currencies t id =
  let of_ id = Option.value_map (Map.find t.totals id) ~default:[] ~f:Map.keys in
  List.dedup_and_sort (of_ id @ of_ (Account_id.pending id)) ~compare:Currency.compare
;;

let find_hold t id = Map.find t.holds id

let open_holds t =
  Map.data t.holds
  |> List.filter ~f:(fun (h : Hold_state.t) -> Amount.is_positive h.remaining)
;;

let is_expired (h : Hold_state.t) ~now =
  Option.exists h.expires_at ~f:(fun expires_at -> Time_ns.( >= ) now expires_at)
;;

let expired_holds t ~now = List.filter (open_holds t) ~f:(is_expired ~now)

let next_expiry t =
  List.filter_map (open_holds t) ~f:(fun h -> h.expires_at)
  |> List.min_elt ~compare:Time_ns.compare
;;

let find_entry t key =
  match Map.find t.keys key with
  | Some (Booked { seq; entry }) -> Some (seq, entry)
  | Some (Rejected _) | None -> None
;;

let find_idempotency_key t key = Option.map (find_entry t key) ~f:fst

let find_rejection t key =
  match Map.find t.keys key with
  | Some (Rejected { seq; reason }) -> Some (seq, reason)
  | Some (Booked _) | None -> None
;;

let reversed_by t key = Map.find t.reversals key
let find_proposal t key = Map.find t.proposals key

let open_proposals t =
  Map.data t.proposals
  |> List.filter ~f:(fun (p : Proposal.t) -> Proposal.Status.equal p.status Proposed)
;;

(* Rows *)

let booked_of_columns kind (c : Columns.t) =
  match Account_kind.normal_side kind with
  | Credit -> Amount.sub_exn c.credits_posted c.debits_posted
  | Debit -> Amount.sub_exn c.debits_posted c.credits_posted
;;

let day_rows t =
  Map.to_alist t.accounts
  |> List.concat_map ~f:(fun (id, account) ->
    List.map (currencies t id) ~f:(fun currency ->
      let c = columns t id currency in
      let at_close = get t.at_close id currency in
      let opening : Columns.t =
        { c with debits_posted = at_close.a; credits_posted = at_close.b }
      in
      { Day_row.account = id
      ; currency
      ; opening_booked = booked_of_columns account.kind opening
      ; debits = Amount.sub_exn c.debits_posted at_close.a
      ; credits = Amount.sub_exn c.credits_posted at_close.b
      ; closing_booked = booked_of_columns account.kind c
      ; closing_available = available t id currency
      }))
;;

(* Validation *)

let fail = Or_error.error_s

let check_time t ~recorded_at =
  match t.last_recorded_at with
  | Some last when Time_ns.(recorded_at < last) ->
    fail
      [%message
        "recorded_at goes backwards"
          (recorded_at : Time_ns.Alternate_sexp.t)
          ~last:(last : Time_ns.Alternate_sexp.t)]
  | _ -> Ok ()
;;

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

let change_status t ~account ~(status : Account_status.t) ~recorded_at =
  let open Or_error.Let_syntax in
  let%bind () = check_time t ~recorded_at in
  match Map.find t.accounts account with
  | None ->
    fail
      [%message "status change for an account that is not open" (account : Account_id.t)]
  | Some _ ->
    let current =
      Map.find t.statuses account |> Option.value ~default:Account_status.Open
    in
    (match current, status with
     | Closed, _ -> fail [%message "account is closed for good" (account : Account_id.t)]
     | _, Closed ->
       (match
          List.find (currencies t account) ~f:(fun c ->
            not (Amount.is_zero (posted t account c) && Amount.is_zero (held t account c)))
        with
        | Some currency ->
          fail
            [%message
              "only an account with nothing booked or held can be closed"
                (account : Account_id.t)
                ~posted:(Amount.to_string (posted t account currency))
                ~held:(Amount.to_string (held t account currency))]
        | None -> Ok ())
     | (Open | Frozen), (Open | Frozen) -> Ok ())
    >>| fun () ->
    { t with
      statuses = Map.set t.statuses ~key:account ~data:status
    ; last_recorded_at = Some recorded_at
    }
;;

let amount_le a b = Amount.compare_value a b |> Or_error.ok_exn <= 0

(* Kind-specific checks. Returns the new hold table, internal flows and reversal links. *)
let check_kind t (entry : Entry.t) ~currency =
  let open Or_error.Let_syntax in
  let postings = entry.postings in
  let touches_pending =
    List.find postings ~f:(fun (p : Posting.t) -> Account_id.is_pending p.account)
  in
  let kind_of id = (Option.value_exn (find_account t id)).kind in
  let out_side_of id = Side.flip (Account_kind.normal_side (kind_of id)) in
  let hold_lookup ?(must_be_expired = false) id =
    match Map.find t.holds id with
    | None -> fail [%message "unknown hold" (id : Hold_id.t)]
    | Some h when not (Amount.is_positive h.remaining) ->
      fail [%message "hold is already closed" (id : Hold_id.t)]
    | Some h when not (Currency.equal (Amount.currency h.remaining) currency) ->
      fail [%message "entry currency differs from the hold's" (id : Hold_id.t)]
    | Some h ->
      let expired = is_expired h ~now:entry.recorded_at in
      if must_be_expired && not expired
      then
        fail
          [%message
            "hold has not expired yet"
              (id : Hold_id.t)
              ~expires_at:(h.expires_at : Time_ns.Alternate_sexp.t option)]
      else if expired && not must_be_expired
      then
        fail
          [%message
            "hold has expired"
              (id : Hold_id.t)
              ~expires_at:(h.expires_at : Time_ns.Alternate_sexp.t option)]
      else Ok h
  in
  (* The hold loses [out_of_pending]; [back] of it returned to the account. *)
  let resolve (h : Hold_state.t) ~out_of_pending ~back =
    if not (amount_le out_of_pending h.remaining)
    then
      fail
        [%message
          "more than the hold's remaining amount"
            ~hold:(h.id : Hold_id.t)
            ~requested:(Amount.to_string out_of_pending)
            ~remaining:(Amount.to_string h.remaining)]
    else
      Ok
        ( Map.set
            t.holds
            ~key:h.id
            ~data:{ h with remaining = Amount.sub_exn h.remaining out_of_pending }
        , bump t.internal h.account currency ~a:(Amount.zero currency) ~b:back
        , t.reversals )
  in
  let split_pending (h : Hold_state.t) =
    let pending_id = Account_id.pending h.account in
    List.partition3_map postings ~f:(fun (p : Posting.t) ->
      if Account_id.equal p.account pending_id
      then `Fst p
      else if Account_id.equal p.account h.account
      then `Snd p
      else `Trd p)
  in
  let place id ~expires_at =
    if Map.mem t.holds id
    then fail [%message "hold id already used" (id : Hold_id.t)]
    else (
      let%bind () =
        match expires_at with
        | Some e when Time_ns.(e <= entry.recorded_at) ->
          fail [%message "hold would expire before it is placed" (id : Hold_id.t)]
        | _ -> Ok ()
      in
      match postings with
      | [ a; b ] ->
        let main, pending = if Account_id.is_pending a.account then b, a else a, b in
        let out = out_side_of main.account in
        if Account_id.equal pending.account (Account_id.pending main.account)
           && Side.equal main.side out
           && Side.equal pending.side (Side.flip out)
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
                  ; placed_by = entry.idempotency_key
                  ; expires_at
                  }
            , bump
                t.internal
                main.account
                currency
                ~a:main.amount
                ~b:(Amount.zero currency)
            , t.reversals )
        else
          fail
            [%message
              "a hold moves funds from an account to its own pending sub-account"
                (id : Hold_id.t)]
      | _ -> fail [%message "a hold entry has exactly two postings" (id : Hold_id.t)])
  in
  let return_to_account ~must_be_expired id =
    let%bind h = hold_lookup ~must_be_expired id in
    let out = out_side_of h.account in
    match split_pending h with
    | [ p ], [ m ], [] when Side.equal p.side out && Side.equal m.side (Side.flip out) ->
      if must_be_expired && not (Amount.equal p.amount h.remaining)
      then fail [%message "an expiry returns the whole remaining amount" (id : Hold_id.t)]
      else resolve h ~out_of_pending:p.amount ~back:m.amount
    | _ ->
      fail
        [%message
          "a release moves funds from the pending sub-account back to the account"
            (id : Hold_id.t)]
  in
  match entry.kind with
  | (Transfer _ | Reversal _) when Option.is_some touches_pending ->
    fail
      [%message
        "only hold entries may post to a pending sub-account"
          (touches_pending : Posting.t option)]
  | Transfer _ -> Ok (t.holds, t.internal, t.reversals)
  | Reversal { reverses } ->
    (match Map.find t.keys reverses with
     | None | Some (Rejected _) ->
       fail [%message "reversal of an unknown entry" (reverses : Idempotency_key.t)]
     | Some (Booked { entry = original; _ }) ->
       (match original.kind, Map.find t.reversals reverses with
        | _, Some by ->
          fail
            [%message
              "entry already reversed"
                (reverses : Idempotency_key.t)
                (by : Idempotency_key.t)]
        | ( ( Hold_place _
            | Hold_release _
            | Hold_capture _
            | Reversal _
            | Hold_place_expiring _
            | Hold_expire _
            | Hold_increment _
            | Hold_over_capture _ )
          , None ) ->
          fail
            [%message
              "only transfers can be reversed; release or capture a hold instead"
                (reverses : Idempotency_key.t)
                (original.kind : Entry.Kind.t)]
        | Transfer _, None ->
          let sorted l = List.sort l ~compare:Posting.compare in
          if [%equal: Posting.t list]
               (sorted postings)
               (sorted (List.map original.postings ~f:Posting.flip))
          then
            Ok
              ( t.holds
              , t.internal
              , Map.set t.reversals ~key:reverses ~data:entry.idempotency_key )
          else
            fail
              [%message
                "a reversal must post the original's postings on the opposite sides"
                  (reverses : Idempotency_key.t)]))
  | Hold_increment id ->
    let%bind h = hold_lookup id in
    let out = out_side_of h.account in
    (match split_pending h with
     | [ p ], [ m ], [] when Side.equal m.side out && Side.equal p.side (Side.flip out) ->
       if not (Amount.equal p.amount m.amount)
       then fail [%message "an increment moves one amount" (id : Hold_id.t)]
       else
         Ok
           ( Map.set
               t.holds
               ~key:id
               ~data:
                 { h with
                   placed = Amount.add_exn h.placed m.amount
                 ; remaining = Amount.add_exn h.remaining m.amount
                 }
           , bump t.internal h.account currency ~a:m.amount ~b:(Amount.zero currency)
           , t.reversals )
     | _ ->
       fail
         [%message
           "an increment moves funds from the account to its pending sub-account"
             (id : Hold_id.t)])
  | Hold_place id -> place id ~expires_at:None
  | Hold_place_expiring { hold; expires_at } -> place hold ~expires_at:(Some expires_at)
  | Hold_release id -> return_to_account ~must_be_expired:false id
  | Hold_expire id -> return_to_account ~must_be_expired:true id
  | Hold_capture id ->
    let%bind h = hold_lookup id in
    let out = out_side_of h.account in
    (match split_pending h with
     | [ p ], back, others
       when Side.equal p.side out
            && List.for_all back ~f:(fun (m : Posting.t) ->
              Side.equal m.side (Side.flip out))
            && List.length back <= 1
            && (not (List.is_empty others))
            && not (List.exists others ~f:(fun o -> Account_id.is_pending o.account)) ->
       let back_amount =
         List.fold back ~init:(Amount.zero currency) ~f:(fun acc m ->
           Amount.add_exn acc m.amount)
       in
       if (not (List.is_empty back)) && not (Amount.equal p.amount h.remaining)
       then
         fail
           [%message
             "a capture that returns the rest must take the whole remaining amount out \
              of the pending sub-account"
               (id : Hold_id.t)]
       else resolve h ~out_of_pending:p.amount ~back:back_amount
     | _ ->
       fail
         [%message
           "a capture moves funds out of the hold's pending sub-account to other accounts"
             (id : Hold_id.t)])
  | Hold_over_capture { hold = id; reason } ->
    let%bind h = hold_lookup id in
    let out = out_side_of h.account in
    if String.is_empty (String.strip reason)
    then fail [%message "an over-capture needs a reason" (id : Hold_id.t)]
    else (
      match split_pending h with
      | [ p ], [ excess ], others
        when Side.equal p.side out
             && Side.equal excess.side out
             && Amount.equal p.amount h.remaining
             && not (List.exists others ~f:(fun o -> Account_id.is_pending o.account)) ->
        resolve h ~out_of_pending:p.amount ~back:(Amount.zero currency)
      | _ ->
        fail
          [%message
            "an over-capture takes the whole hold plus an excess from the account"
              (id : Hold_id.t)])
;;

(* Frozen and closed accounts accept money in and the return of held funds, nothing else.
   Measured on the account and its pending sub-account together, so placing a hold (a move
   between the two) counts as going out. *)
let check_status t (entry : Entry.t) =
  let returns_held =
    match entry.kind with
    | Hold_release _ | Hold_expire _ -> true
    | _ -> false
  in
  let places_hold =
    match entry.kind with
    | Hold_place _ | Hold_place_expiring _ -> true
    | _ -> false
  in
  List.map entry.postings ~f:(fun p -> main_account p.account)
  |> List.dedup_and_sort ~compare:Account_id.compare
  |> List.find_map ~f:(fun id ->
    match status t id with
    | Open -> None
    | (Frozen | Closed) when returns_held -> None
    | Closed -> Some (id, Account_status.Closed)
    | Frozen ->
      let kind = (Option.value_exn (find_account t id)).kind in
      let net =
        List.fold
          entry.postings
          ~init:(Amount.zero (Entry.currency entry))
          ~f:(fun acc p ->
            if Account_id.equal (main_account p.account) id
            then Amount.add_exn acc (normal kind (Posting.signed p))
            else acc)
      in
      if places_hold || Amount.is_negative net then Some (id, Frozen) else None)
  |> function
  | None -> Ok ()
  | Some (account, status) ->
    fail
      [%message
        "account does not accept this entry"
          (account : Account_id.t)
          (status : Account_status.t)
          ~idempotency_key:(entry.idempotency_key : Idempotency_key.t)]
;;

let post ?(approved_adjustment = false) t ~seq (entry : Entry.t) =
  let open Or_error.Let_syntax in
  let%bind () =
    match entry.code with
    | Adjustment _ when not approved_adjustment ->
      fail
        [%message
          "adjustments are booked only through proposal and approval"
            ~idempotency_key:(entry.idempotency_key : Idempotency_key.t)]
    | _ -> Ok ()
  in
  let%bind () =
    match Map.find t.keys entry.idempotency_key with
    | Some (Booked { seq = booked_at; _ }) ->
      fail
        [%message
          "idempotency key already booked"
            ~idempotency_key:(entry.idempotency_key : Idempotency_key.t)
            (booked_at : int)]
    | Some (Rejected { seq = refused_at; reason }) ->
      fail
        [%message
          "idempotency key already refused"
            ~idempotency_key:(entry.idempotency_key : Idempotency_key.t)
            (refused_at : int)
            reason]
    | None -> Ok ()
  in
  let%bind () = check_time t ~recorded_at:entry.recorded_at in
  let%bind () =
    match t.last_business_date, t.closed_through with
    | _, Some closed when Date.(entry.business_date <= closed) ->
      fail
        [%message
          "business date already closed"
            (entry.business_date : Date.t)
            ~closed_through:(closed : Date.t)]
    | Some last, _ when Date.(entry.business_date < last) ->
      fail
        [%message
          "business date goes backwards" (entry.business_date : Date.t) (last : Date.t)]
    | _ -> Ok ()
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
  let%bind () = check_status t entry in
  let currency = Entry.currency entry in
  let%bind holds, internal, reversals = check_kind t entry ~currency in
  let totals =
    List.fold entry.postings ~init:t.totals ~f:(fun totals p ->
      let zero = Amount.zero currency in
      match p.side with
      | Debit -> bump totals p.account currency ~a:p.amount ~b:zero
      | Credit -> bump totals p.account currency ~a:zero ~b:p.amount)
  in
  let t' =
    { t with
      totals
    ; internal
    ; holds
    ; reversals
    ; keys = Map.set t.keys ~key:entry.idempotency_key ~data:(Booked { seq; entry })
    ; last_recorded_at = Some entry.recorded_at
    ; last_business_date = Some entry.business_date
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

let reject t ~seq ~idempotency_key ~reason ~recorded_at =
  let open Or_error.Let_syntax in
  let%bind () = check_time t ~recorded_at in
  if Map.mem t.keys idempotency_key
  then
    fail
      [%message
        "cannot record a refusal for a key already used"
          (idempotency_key : Idempotency_key.t)]
  else
    Ok
      { t with
        keys = Map.set t.keys ~key:idempotency_key ~data:(Rejected { seq; reason })
      ; last_recorded_at = Some recorded_at
      }
;;

let close_day t ~business_date ~recorded_at =
  let open Or_error.Let_syntax in
  let%bind () = check_time t ~recorded_at in
  match t.closed_through with
  | Some closed when Date.(business_date <= closed) ->
    fail
      [%message "business date already closed" (business_date : Date.t) (closed : Date.t)]
  | _ ->
    let at_close =
      Map.fold t.accounts ~init:Account_id.Map.empty ~f:(fun ~key:id ~data:_ acc ->
        List.fold (currencies t id) ~init:acc ~f:(fun acc currency ->
          let c = columns t id currency in
          let zero = Amount.zero currency in
          bump acc id currency ~a:c.debits_posted ~b:zero
          |> fun acc -> bump acc id currency ~a:zero ~b:c.credits_posted))
    in
    Ok
      { t with
        at_close
      ; last_close_rows = day_rows t
      ; closed_through = Some business_date
      ; last_recorded_at = Some recorded_at
      }
;;

let propose t ~(entry : Entry.t) ~maker ~reason ~ticket =
  let open Or_error.Let_syntax in
  let key = entry.idempotency_key in
  let%bind () = check_time t ~recorded_at:entry.recorded_at in
  if List.exists [ maker; reason; ticket ] ~f:(fun s -> String.is_empty (String.strip s))
  then fail [%message "a proposal needs a maker, a reason and a ticket"]
  else if Map.mem t.keys key || Map.mem t.proposals key
  then
    fail
      [%message "idempotency key already used" ~idempotency_key:(key : Idempotency_key.t)]
  else
    Ok
      { t with
        proposals =
          Map.set
            t.proposals
            ~key
            ~data:
              { Proposal.entry
              ; maker
              ; reason
              ; ticket
              ; proposed_at = entry.recorded_at
              ; status = Proposed
              }
      ; last_recorded_at = Some entry.recorded_at
      }
;;

let decidable t key ~now =
  match Map.find t.proposals key with
  | None -> fail [%message "no such proposal" ~idempotency_key:(key : Idempotency_key.t)]
  | Some (p : Proposal.t) when not (Proposal.Status.equal p.status Proposed) ->
    fail
      [%message
        "proposal already decided"
          ~idempotency_key:(key : Idempotency_key.t)
          ~status:(p.status : Proposal.Status.t)]
  | Some p when Proposal.is_expired p ~now ->
    fail [%message "proposal expired" ~idempotency_key:(key : Idempotency_key.t)]
  | Some p -> Ok p
;;

let approve t ~seq (entry : Entry.t) =
  let open Or_error.Let_syntax in
  let key = entry.idempotency_key in
  let%bind p = decidable t key ~now:entry.recorded_at in
  let%bind () =
    match entry.code with
    | Adjustment { maker; checker; reason; ticket } ->
      let%bind () =
        if String.equal maker p.maker
           && String.equal reason p.reason
           && String.equal ticket p.ticket
        then Ok ()
        else
          fail [%message "approval does not match the proposal's maker, reason or ticket"]
      in
      if String.equal checker p.maker
      then fail [%message "the maker cannot approve their own adjustment" checker]
      else (
        (* Apart from the code and the ledger's stamps, the booked entry is the proposed
           one. *)
        let as_proposed =
          Entry.with_code
            (Entry.restamp
               entry
               ~recorded_at:p.entry.recorded_at
               ~business_date:p.entry.business_date)
            p.entry.code
        in
        match as_proposed with
        | Ok e when Entry.equal e p.entry -> Ok ()
        | _ -> fail [%message "approved entry differs from the proposal"])
    | _ -> fail [%message "an approved adjustment carries the Adjustment code"]
  in
  let%map t = post ~approved_adjustment:true t ~seq entry in
  { t with proposals = Map.set t.proposals ~key ~data:{ p with status = Approved } }
;;

let reject_proposal t ~seq ~idempotency_key ~checker ~recorded_at =
  let open Or_error.Let_syntax in
  let%bind () = check_time t ~recorded_at in
  (* Rejecting stays possible after expiry: it tidies up. *)
  match Map.find t.proposals idempotency_key with
  | Some p when Proposal.Status.equal p.status Proposed ->
    Ok
      { t with
        proposals =
          Map.set t.proposals ~key:idempotency_key ~data:{ p with status = Rejected }
      ; keys =
          Map.set
            t.keys
            ~key:idempotency_key
            ~data:(Rejected { seq; reason = sprintf "adjustment rejected by %s" checker })
      ; last_recorded_at = Some recorded_at
      }
  | _ ->
    let%bind _ = decidable t idempotency_key ~now:recorded_at in
    fail [%message "unreachable"]
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
      | Linked [] -> fail [%message "empty linked group"]
      | Linked entries ->
        (* All or nothing: a failure anywhere discards the partly applied state. *)
        List.fold_result entries ~init:t ~f:(fun t entry ->
          post t ~seq entry
          |> Or_error.tag_s
               ~tag:
                 [%message
                   "linked entry refused, so the whole group is"
                     ~idempotency_key:(entry.idempotency_key : Idempotency_key.t)])
      | Account_status_changed { account; status; recorded_at } ->
        change_status t ~account ~status ~recorded_at
      | Rejected { idempotency_key; reason; recorded_at } ->
        reject t ~seq ~idempotency_key ~reason ~recorded_at
      | Day_closed { business_date; recorded_at } ->
        close_day t ~business_date ~recorded_at
      | Adjustment_proposed { entry; maker; reason; ticket } ->
        propose t ~entry ~maker ~reason ~ticket
      | Adjustment_approved entry -> approve t ~seq entry
      | Adjustment_rejected { idempotency_key; checker; recorded_at } ->
        reject_proposal t ~seq ~idempotency_key ~checker ~recorded_at
    in
    { t with next_seq = seq + 1 })
;;

let of_records records = List.fold_result records ~init:empty ~f:apply

type state = t

(* Serialization: the maps as sorted association lists of stable types. *)
module Repr = struct
  module Pair = struct
    type t =
      Account_id.Stable.V1.t
      * Currency.Stable.V1.t
      * Amount.Stable.V1.t
      * Amount.Stable.V1.t
    [@@deriving bin_io, sexp]
  end

  module Key = struct
    type t =
      | Booked of
          { seq : int
          ; entry : Entry.Stable.V1.t
          }
      | Rejected of
          { seq : int
          ; reason : string
          }
    [@@deriving bin_io, sexp]
  end

  type t =
    { accounts : Account.Stable.V1.t list
    ; statuses : (Account_id.Stable.V1.t * Account_status.Stable.V1.t) list
    ; totals : Pair.t list
    ; internal : Pair.t list
    ; at_close : Pair.t list
    ; holds : Hold_state.Stable.V1.t list
    ; proposals : (Idempotency_key.Stable.V1.t * Proposal.Stable.V1.t) list
    ; keys : (Idempotency_key.Stable.V1.t * Key.t) list
    ; reversals : (Idempotency_key.Stable.V1.t * Idempotency_key.Stable.V1.t) list
    ; next_seq : int
    ; last_recorded_at : Time_ns.Stable.Alternate_sexp.V1.t option
    ; last_business_date : Date.Stable.V1.t option
    ; closed_through : Date.Stable.V1.t option
    ; last_close_rows : Day_row.Stable.V1.t list
    }
  [@@deriving bin_io, sexp]

  let pairs_of (m : by_account) =
    Map.to_alist m
    |> List.concat_map ~f:(fun (id, by_currency) ->
      Map.to_alist by_currency |> List.map ~f:(fun (c, p) -> id, c, p.a, p.b))
  ;;

  let of_pairs pairs =
    List.fold pairs ~init:Account_id.Map.empty ~f:(fun m (id, c, a, b) ->
      bump m id c ~a ~b)
  ;;

  let of_t (t : state) : t =
    { accounts = Map.data t.accounts
    ; statuses = Map.to_alist t.statuses
    ; totals = pairs_of t.totals
    ; internal = pairs_of t.internal
    ; at_close = pairs_of t.at_close
    ; holds = Map.data t.holds
    ; proposals = Map.to_alist t.proposals
    ; keys =
        Map.to_alist t.keys
        |> List.map ~f:(fun (k, (v : Key_state.t)) ->
          ( k
          , match v with
            | Booked { seq; entry } -> Key.Booked { seq; entry }
            | Rejected { seq; reason } -> Rejected { seq; reason } ))
    ; reversals = Map.to_alist t.reversals
    ; next_seq = t.next_seq
    ; last_recorded_at = t.last_recorded_at
    ; last_business_date = t.last_business_date
    ; closed_through = t.closed_through
    ; last_close_rows = t.last_close_rows
    }
  ;;

  let to_t (r : t) : state =
    { accounts =
        Account_id.Map.of_alist_exn (List.map r.accounts ~f:(fun a -> a.Account.id, a))
    ; statuses = Account_id.Map.of_alist_exn r.statuses
    ; totals = of_pairs r.totals
    ; internal = of_pairs r.internal
    ; at_close = of_pairs r.at_close
    ; holds = Hold_id.Map.of_alist_exn (List.map r.holds ~f:(fun h -> h.id, h))
    ; proposals = Idempotency_key.Map.of_alist_exn r.proposals
    ; keys =
        Idempotency_key.Map.of_alist_exn
          (List.map r.keys ~f:(fun (k, v) ->
             ( k
             , match v with
               | Key.Booked { seq; entry } -> Key_state.Booked { seq; entry }
               | Rejected { seq; reason } -> Rejected { seq; reason } )))
    ; reversals = Idempotency_key.Map.of_alist_exn r.reversals
    ; next_seq = r.next_seq
    ; last_recorded_at = r.last_recorded_at
    ; last_business_date = r.last_business_date
    ; closed_through = r.closed_through
    ; last_close_rows = r.last_close_rows
    }
  ;;
end

module Stable = struct
  module V1 = struct
    type nonrec t = t

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type nonrec t = t

          let to_binable = Repr.of_t
          let of_binable = Repr.to_t
        end)

    let sexp_of_t t = Repr.sexp_of_t (Repr.of_t t)
  end
end

let equal a b =
  String.equal
    (Binable.to_string (module Stable.V1) a)
    (Binable.to_string (module Stable.V1) b)
;;
