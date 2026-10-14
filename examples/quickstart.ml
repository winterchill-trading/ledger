(* The worked example of the documentation: a chart of accounts, a deposit booked once
   even when retried, a card authorisation captured later, the trial balance, and the
   chain head to publish. Run with [dune exec examples/quickstart.exe]. *)

open! Core
open Ledger

let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn
let id = Account_id.of_string
let key = Idempotency_key.of_string

let () =
  let dir = Filename_unix.temp_dir "ledger" "quickstart" in
  let j = Journal.create (dir ^/ "bank.journal") |> Or_error.ok_exn in
  let book r = ignore (Or_error.ok_exn r : Journal.Append_result.t) in
  (* Asset accounts are what the bank holds, liability accounts what it owes. *)
  book (Journal.open_account j (Account.create (id "bank:central") Asset));
  book
    (Journal.open_account
       j
       (Account.create (id "external:cards") Asset ~allow_negative:true));
  book (Journal.open_account j (Account.create (id "customer:alice") Liability));
  (* The value date comes with the provider's event, so a retry carries the same one. *)
  let value_date = Journal.now j in
  let deposit () =
    Journal.transfer
      j
      ~idempotency_key:(key "provider-event-123")
      ~tag:"deposit"
      ~effective_at:value_date
      [ Posting.debit (id "bank:central") (eur "100.00")
      ; Posting.credit (id "customer:alice") (eur "100.00")
      ]
  in
  book (deposit ());
  (* The same event delivered twice books nothing the second time. *)
  (match deposit () with
   | Ok (Duplicate { seq }) -> printf "duplicate of record %d\n" seq
   | Ok (Appended _) | Error _ -> assert false);
  (* A card authorisation reserves funds; the settlement captures them. *)
  let auth = Hold_id.of_string "auth-9f2" in
  book
    (Journal.hold
       j
       ~hold_id:auth
       ~account:(id "customer:alice")
       ~amount:(eur "12.00")
       ~timeout:(Time_ns.Span.of_day 7.)
       ~idempotency_key:(key "auth-9f2")
       ~effective_at:(Journal.now j)
       ());
  book
    (Journal.capture
       j
       ~hold_id:auth
       ~to_:(id "external:cards")
       ~idempotency_key:(key "settle-9f2")
       ~effective_at:(Journal.now j)
       ());
  let b = Journal.balances j in
  printf
    "alice available: %s\n"
    (Money.Amount.to_string
       (Balances.available b (id "customer:alice") Money.Currency.eur));
  let tb = Trial_balance.of_balances b in
  print_string (Trial_balance.to_string_hum tb);
  Trial_balance.check tb |> Or_error.ok_exn;
  (* Keep this line somewhere outside the system; [Chain_head.verify] checks it later. *)
  print_endline (Chain_head.to_string (Journal.chain_head j));
  Journal.close j
;;
