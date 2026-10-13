open! Core
open Ledger

(* The usage example from README.md, with the file in a temporary directory. Keep the two
   in step. *)
let%expect_test "README example" =
  let dir = Filename_unix.temp_dir "ledger" "readme" in
  let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn in
  let id = Account_id.of_string in
  let key = Idempotency_key.of_string in
  let j = Journal.create (dir ^/ "bank.journal") |> Or_error.ok_exn in
  let ok r = ignore (Or_error.ok_exn r : Journal.Append_result.t) in
  ok (Journal.open_account j (Account.create (id "bank:central") Asset));
  ok
    (Journal.open_account
       j
       (Account.create (id "external:cards") Asset ~allow_negative:true));
  ok (Journal.open_account j (Account.create (id "customer:alice") Liability));
  ok
    (Journal.transfer
       j
       ~idempotency_key:(key "provider-event-123")
       ~tag:"deposit"
       ~effective_at:(Journal.now j)
       [ Posting.debit (id "bank:central") (eur "100.00")
       ; Posting.credit (id "customer:alice") (eur "100.00")
       ]);
  let auth = Hold_id.of_string "auth-9f2" in
  ok
    (Journal.hold
       j
       ~hold_id:auth
       ~account:(id "customer:alice")
       ~amount:(eur "12.00")
       ~idempotency_key:(key "auth-9f2")
       ~effective_at:(Journal.now j)
       ());
  ok
    (Journal.capture
       j
       ~hold_id:auth
       ~to_:(id "external:cards")
       ~idempotency_key:(key "settle-9f2")
       ~effective_at:(Journal.now j)
       ());
  let b = Journal.balances j in
  print_endline
    (Money.Amount.to_string
       (Balances.available b (id "customer:alice") Money.Currency.eur));
  Trial_balance.of_balances b |> Trial_balance.check |> Or_error.ok_exn;
  Journal.close j;
  Core_unix.unlink (dir ^/ "bank.journal");
  Core_unix.rmdir dir;
  [%expect {| 88.00 EUR |}]
;;
