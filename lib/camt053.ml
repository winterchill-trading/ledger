open! Core
module Ledger_entry = Entry
module Amount = Money.Amount
module Currency = Money.Currency

module Indicator = struct
  type t =
    | Crdt
    | Dbit
  [@@deriving sexp_of, compare, equal]
end

module Status = struct
  type t =
    | Book
    | Pdng
  [@@deriving sexp_of, compare, equal]
end

module Balance = struct
  module Type = struct
    type t =
      | Opbd
      | Clbd
      | Clav
    [@@deriving sexp_of, compare, equal]
  end

  type t =
    { type_ : Type.t
    ; amount : Amount.t
    ; indicator : Indicator.t
    ; date : Date.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Entry = struct
  type t =
    { entry_ref : string
    ; amount : Amount.t
    ; indicator : Indicator.t
    ; reversal : bool
    ; status : Status.t
    ; booking_date : Date.t option
    ; value_date : Date.t
    ; account_servicer_ref : string
    ; bank_transaction_code : string
    ; end_to_end_id : string
    ; remittance_info : string
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { statement_id : string
  ; created_at : Time_ns.Alternate_sexp.t
  ; account : Account_id.t
  ; currency : Currency.t
  ; from_date : Date.t
  ; to_date : Date.t
  ; balances : Balance.t list
  ; entries : Entry.t list
  }
[@@deriving sexp_of, compare, equal]

let utc_date time = Time_ns.to_date time ~zone:Time_float.Zone.utc

(* A balance on the account's normal side as an amount and an indicator. *)
let balance_of (kind : Account_kind.t) type_ amount date =
  let positive_indicator : Indicator.t =
    match Account_kind.normal_side kind with
    | Credit -> Crdt
    | Debit -> Dbit
  in
  let indicator : Indicator.t =
    if Amount.is_negative amount
    then (
      match positive_indicator with
      | Crdt -> Dbit
      | Dbit -> Crdt)
    else positive_indicator
  in
  { Balance.type_; amount = Amount.abs amount; indicator; date }
;;

let record_date (r : Record.t) =
  List.hd (Record.entries r)
  |> Option.map ~f:(fun (e : Ledger_entry.t) -> e.business_date)
;;

let create
  ?(include_pending = false)
  ?statement_id
  records
  ~account
  ~currency
  ~from_date
  ~to_date
  ~created_at
  =
  let open Or_error.Let_syntax in
  let pending_id = Account_id.pending account in
  let in_range d = Date.(d >= from_date && d <= to_date) in
  (* One pass: [state] is the fold so far; the opening balance is taken just before the
     first record dated inside the range, the closing state just before the first one
     dated after it. *)
  let rec go state ~opening ~entries = function
    | [] -> Ok (state, opening, entries)
    | (r : Record.t) :: rest ->
      (match record_date r with
       | Some d when Date.(d > to_date) -> Ok (state, opening, entries)
       | date ->
         let opening =
           match opening, date with
           | None, Some d when in_range d -> Some (Balances.posted state account currency)
           | _ -> opening
         in
         let%bind next = Balances.apply state r in
         let entries =
           List.foldi
             (Record.entries r)
             ~init:entries
             ~f:(fun i acc (e : Ledger_entry.t) ->
               if not
                    (in_range e.business_date
                     && Currency.equal (Ledger_entry.currency e) currency)
               then acc
               else (
                 let net =
                   List.fold e.postings ~init:(Amount.zero currency) ~f:(fun acc p ->
                     if Account_id.equal p.account account
                        || Account_id.equal p.account pending_id
                     then Amount.add_exn acc (Posting.signed p)
                     else acc)
                 in
                 if Amount.is_zero net
                 then acc
                 else
                   { Entry.entry_ref = sprintf "%d.%d" r.seq i
                   ; amount = Amount.abs net
                   ; indicator = (if Amount.is_positive net then Dbit else Crdt)
                   ; reversal =
                       (match e.kind with
                        | Reversal _ -> true
                        | _ -> false)
                   ; status = Book
                   ; booking_date = Some e.business_date
                   ; value_date = utc_date e.effective_at
                   ; account_servicer_ref = Idempotency_key.to_string e.idempotency_key
                   ; bank_transaction_code = Ledger_entry.Code.to_string e.code
                   ; end_to_end_id = e.reference
                   ; remittance_info = e.remittance_info
                   }
                   :: acc))
         in
         go next ~opening ~entries rest)
  in
  let%bind state, opening, entries =
    go Balances.empty ~opening:None ~entries:[] records
  in
  match Balances.find_account state account with
  | None -> Or_error.error_s [%message "account not opened" (account : Account_id.t)]
  | Some { kind; _ } ->
    let closing = Balances.posted state account currency in
    let opening = Option.value opening ~default:closing in
    let pending =
      if not include_pending
      then []
      else
        Balances.open_holds state
        |> List.filter ~f:(fun (h : Balances.Hold_state.t) ->
          Account_id.equal h.account account
          && Currency.equal (Amount.currency h.remaining) currency)
        |> List.filter_map ~f:(fun (h : Balances.Hold_state.t) ->
          Option.map
            (Balances.find_entry state h.placed_by)
            ~f:(fun (seq, (e : Ledger_entry.t)) ->
              { Entry.entry_ref = sprintf "%d.hold" seq
              ; amount = h.remaining
              ; indicator =
                  (match Account_kind.normal_side kind with
                   | Credit -> Dbit
                   | Debit -> Crdt)
              ; reversal = false
              ; status = Pdng
              ; booking_date = None
              ; value_date = utc_date e.effective_at
              ; account_servicer_ref = Idempotency_key.to_string e.idempotency_key
              ; bank_transaction_code = Ledger_entry.Code.to_string e.code
              ; end_to_end_id = e.reference
              ; remittance_info = e.remittance_info
              }))
    in
    Ok
      { statement_id =
          Option.value
            statement_id
            ~default:
              (sprintf
                 "%s/%s/%s"
                 (Account_id.to_string account)
                 (Currency.to_string currency)
                 (Date.to_string from_date))
      ; created_at
      ; account
      ; currency
      ; from_date
      ; to_date
      ; balances =
          [ balance_of kind Opbd opening from_date
          ; balance_of kind Clbd closing to_date
          ; balance_of kind Clav (Balances.available state account currency) to_date
          ]
      ; entries = List.rev entries @ pending
      }
;;

let to_string_hum t =
  let ind : Indicator.t -> string = function
    | Crdt -> "CRDT"
    | Dbit -> "DBIT"
  in
  let bal (b : Balance.t) =
    sprintf
      "  %s %s %s %s"
      (match b.type_ with
       | Opbd -> "OPBD"
       | Clbd -> "CLBD"
       | Clav -> "CLAV")
      (Date.to_string b.date)
      (ind b.indicator)
      (Amount.to_string b.amount)
  in
  let entry (e : Entry.t) =
    sprintf
      "  %-8s %s %-10s %-10s %s %16s %-17s %-12s %s%s"
      e.entry_ref
      (match e.status with
       | Book -> "BOOK"
       | Pdng -> "PDNG")
      (Option.value_map e.booking_date ~default:"-" ~f:Date.to_string)
      (Date.to_string e.value_date)
      (ind e.indicator)
      (Amount.to_string e.amount)
      e.bank_transaction_code
      e.end_to_end_id
      e.remittance_info
      (if e.reversal then " (reversal)" else "")
    |> String.rstrip
  in
  String.concat
    ~sep:"\n"
    ([ sprintf
         "camt.053 %s %s %s %s..%s"
         t.statement_id
         (Account_id.to_string t.account)
         (Currency.to_string t.currency)
         (Date.to_string t.from_date)
         (Date.to_string t.to_date)
     ]
     @ List.map t.balances ~f:bal
     @ List.map t.entries ~f:entry)
  ^ "\n"
;;
