open! Core
module Amount = Money.Amount
module Currency = Money.Currency

module Line = struct
  type t =
    { seq : int
    ; effective_at : Time_ns.Alternate_sexp.t
    ; recorded_at : Time_ns.Alternate_sexp.t
    ; kind : Entry.Kind.t
    ; reference : string
    ; side : Side.t
    ; amount : Amount.t
    ; balance : Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { account : Account_id.t
  ; currency : Currency.t
  ; opening : Amount.t
  ; lines : Line.t list
  ; closing : Amount.t
  ; next_after_seq : int option
  }
[@@deriving sexp_of, compare, equal]

let find_kind records account =
  let owner = Option.value (Account_id.parent_of_pending account) ~default:account in
  List.find_map records ~f:(fun (r : Record.t) ->
    match r.event with
    | Account_opened a when Account_id.equal a.id owner -> Some a.kind
    | _ -> None)
;;

let create ?from ?until ?(after_seq = 0) ?limit records ~account ~currency =
  match find_kind records account with
  | None -> Or_error.error_s [%message "account not opened" (account : Account_id.t)]
  | Some kind ->
    let normal_side = Account_kind.normal_side kind in
    let in_range (e : Entry.t) =
      Option.for_all from ~f:(fun from -> Time_ns.(e.effective_at >= from))
      && Option.for_all until ~f:(fun until -> Time_ns.(e.effective_at < until))
    in
    let limit = Option.value limit ~default:Int.max_value in
    (* One pass: [balance] counts every posting; lines are kept only for entries in the
       page. [entries] counts entries on the page; [more] notes a qualifying entry beyond
       it. *)
    let balance = ref (Amount.zero currency) in
    let opening = ref None in
    let lines = ref [] in
    let entries = ref 0 in
    let more = ref false in
    let last_seq = ref after_seq in
    List.iter records ~f:(fun (r : Record.t) ->
      List.iter (Record.entries r) ~f:(fun entry ->
        let mine =
          List.filter entry.postings ~f:(fun (p : Posting.t) ->
            Account_id.equal p.account account
            && Currency.equal (Amount.currency p.amount) currency)
        in
        if not (List.is_empty mine)
        then (
          let wanted = r.seq > after_seq && in_range entry in
          if wanted && !entries >= limit then more := true;
          let on_page = wanted && !entries < limit in
          if on_page
          then (
            if Option.is_none !opening then opening := Some !balance;
            incr entries;
            last_seq := r.seq);
          List.iter mine ~f:(fun p ->
            let delta =
              if Side.equal p.side normal_side then p.amount else Amount.neg p.amount
            in
            balance := Amount.add_exn !balance delta;
            if on_page
            then
              lines
              := { Line.seq = r.seq
                 ; effective_at = entry.effective_at
                 ; recorded_at = entry.recorded_at
                 ; kind = entry.kind
                 ; reference = entry.reference
                 ; side = p.side
                 ; amount = p.amount
                 ; balance = !balance
                 }
                 :: !lines))));
    let lines = List.rev !lines in
    let closing =
      match List.last lines with
      | Some l -> l.balance
      | None -> !balance
    in
    Ok
      { account
      ; currency
      ; opening = Option.value !opening ~default:closing
      ; lines
      ; closing
      ; next_after_seq = Option.some_if !more !last_seq
      }
;;

let kind_to_string : Entry.Kind.t -> string = function
  | Transfer tag -> tag
  | Hold_place id -> "hold " ^ Hold_id.to_string id
  | Hold_release id -> "release " ^ Hold_id.to_string id
  | Hold_capture id -> "capture " ^ Hold_id.to_string id
  | Reversal { reverses } -> "reversal of " ^ Idempotency_key.to_string reverses
  | Hold_place_expiring { hold; _ } -> "hold " ^ Hold_id.to_string hold
  | Hold_expire id -> "expiry " ^ Hold_id.to_string id
  | Hold_increment id -> "hold+ " ^ Hold_id.to_string id
  | Hold_over_capture { hold; _ } -> "capture+ " ^ Hold_id.to_string hold
;;

let to_string_hum t =
  let time ts = Time_ns.to_sec_string ts ~zone:Time_float.Zone.utc in
  let row seq date kind reference debit credit balance =
    sprintf
      "%5s  %-19s  %-18s %-14s %14s %14s %14s"
      seq
      date
      kind
      reference
      debit
      credit
      balance
  in
  let amount = Amount.to_decimal_string in
  let lines =
    List.map t.lines ~f:(fun (l : Line.t) ->
      let debit, credit =
        match l.side with
        | Debit -> amount l.amount, ""
        | Credit -> "", amount l.amount
      in
      row
        (Int.to_string l.seq)
        (time l.effective_at)
        (kind_to_string l.kind)
        l.reference
        debit
        credit
        (amount l.balance))
  in
  String.concat
    ~sep:"\n"
    ([ sprintf
         "Statement %s %s"
         (Account_id.to_string t.account)
         (Currency.to_string t.currency)
     ; row "SEQ" "EFFECTIVE (UTC)" "KIND" "REFERENCE" "DEBIT" "CREDIT" "BALANCE"
     ; row "" "" "opening balance" "" "" "" (amount t.opening)
     ]
     @ lines
     @ [ row "" "" "closing balance" "" "" "" (amount t.closing) ]
     @
     match t.next_after_seq with
     | None -> []
     | Some seq -> [ sprintf "more: -after %d" seq ])
  ^ "\n"
;;
