open! Core
open Ledger

let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn
let btc s = Money.Amount.of_decimal_string Money.Currency.btc s |> Or_error.ok_exn
let id = Account_id.of_string
let key = Idempotency_key.of_string

(* Fixed instants so expect output does not depend on the clock or the time zone. *)
let epoch = Time_ns.of_string_with_utc_offset "2026-10-01 09:00:00Z"
let at minutes = Time_ns.add epoch (Time_ns.Span.of_min (Float.of_int minutes))

(* A small bank: cash at the central bank and a settlement account at the payment network
   (both may go negative while money is in flight), fee income, and customers. *)
let central_bank = id "bank:central"
let network = id "external:sepa"
let fees = id "income:fees"
let card_scheme = id "external:cards"
let alice = id "customer:alice"
let bob = id "customer:bob"

let chart =
  [ Account.create central_bank Asset
  ; Account.create network Asset ~allow_negative:true
  ; Account.create card_scheme Asset ~allow_negative:true
  ; Account.create fees Income
  ; Account.create alice Liability ~tags:[ "customer", "alice" ]
  ; Account.create bob Liability ~tags:[ "customer", "bob" ]
  ]
;;

let ok_exn = Or_error.ok_exn

let open_chart balances =
  List.fold chart ~init:balances ~f:(fun b account ->
    Balances.apply b { seq = Balances.next_seq b; event = Account_opened account }
    |> ok_exn)
;;

let entry ?(kind = Entry.Kind.Transfer "transfer") ?reference ?(at = at 0) k postings =
  Entry.create
    ~idempotency_key:(key k)
    ~kind
    ?reference
    ~effective_at:at
    ~recorded_at:at
    postings
;;

let post balances entry =
  let%bind.Or_error entry in
  Balances.apply balances { seq = Balances.next_seq balances; event = Posted entry }
;;

let show_balance b account currency =
  print_s
    [%message
      ""
        ~_:(account : Account_id.t)
        ~available:(Money.Amount.to_string (Balances.available b account currency))
        ~held:(Money.Amount.to_string (Balances.held b account currency))
        ~posted:(Money.Amount.to_string (Balances.posted b account currency))]
;;

(* [dir] (a temporary directory) is printed as $DIR so expect output is stable. *)
let print_error ?dir = function
  | Ok _ -> print_endline "ok"
  | Error e ->
    (* Replace inside the atoms, before printing, so line wrapping does not depend on how
       long the temporary path is (macOS and Linux differ). *)
    let rec mask : Sexp.t -> Sexp.t = function
      | Atom a ->
        Atom
          (match dir with
           | None -> a
           | Some dir -> String.substr_replace_all a ~pattern:dir ~with_:"$DIR")
      | List l -> List (List.map l ~f:mask)
    in
    print_s (mask [%sexp (e : Error.t)])
;;
