open! Core
open Ledger
open Helpers

let%expect_test "entries must balance in one currency" =
  let show postings = print_error (entry "k" postings) in
  show [ Posting.debit central_bank (eur "10.00"); Posting.credit alice (eur "10.00") ];
  show [ Posting.debit central_bank (eur "10.00"); Posting.credit alice (eur "9.99") ];
  show [ Posting.debit central_bank (eur "10.00") ];
  show [];
  show [ Posting.debit central_bank (eur "1.00"); Posting.credit alice (btc "1.00") ];
  show [ Posting.debit central_bank (eur "0.00"); Posting.credit alice (eur "0.00") ];
  show [ Posting.debit central_bank (eur "-1.00"); Posting.credit alice (eur "-1.00") ];
  (* Several postings per side are fine. *)
  show
    [ Posting.debit central_bank (eur "10.00")
    ; Posting.credit alice (eur "9.50")
    ; Posting.credit fees (eur "0.50")
    ];
  [%expect
    {|
    ok
    ("unbalanced entry: debits minus credits is not zero"
     (debits_minus_credits "0.01 EUR") (idempotency_key k))
    ("an entry needs at least two postings" (idempotency_key k))
    ("an entry needs at least two postings" (idempotency_key k))
    (("postings in more than one currency"
      (p ((account customer:alice) (side Credit) (amount "1.00000000 BTC"))))
     (idempotency_key k))
    (("posting amount must be positive"
      (p ((account bank:central) (side Debit) (amount "0.00 EUR"))))
     (idempotency_key k))
    (("posting amount must be positive"
      (p ((account bank:central) (side Debit) (amount "-1.00 EUR"))))
     (idempotency_key k))
    ok
    |}]
;;

let%expect_test "total and currency" =
  let e =
    entry
      "k"
      [ Posting.debit central_bank (eur "10.00")
      ; Posting.credit alice (eur "9.50")
      ; Posting.credit fees (eur "0.50")
      ]
    |> ok_exn
  in
  print_s
    [%message
      (Money.Amount.to_string (Entry.total e) : string)
        (Entry.currency e : Money.Currency.t)];
  [%expect
    {|
    (("Money.Amount.to_string (Entry.total e)" "10.00 EUR")
     ("Entry.currency e" EUR))
    |}]
;;

let%expect_test "decoding validates again" =
  let good =
    entry
      "k"
      [ Posting.debit central_bank (eur "1.00"); Posting.credit alice (eur "1.00") ]
    |> ok_exn
  in
  let sexp = Entry.Stable.V1.sexp_of_t good in
  print_s sexp;
  (* Change the credit to 2.00 by editing the sexp, as a hand-edited journal would. *)
  let tampered =
    Sexp.to_string sexp
    |> String.substr_replace_all
         ~pattern:"(side Credit)(amount\"1.00 EUR\")"
         ~with_:"(side Credit)(amount\"2.00 EUR\")"
    |> Sexp.of_string
  in
  Expect_test_helpers_core.require_does_raise (fun () ->
    Entry.Stable.V1.t_of_sexp tampered);
  let bin = Binable.to_string (module Entry.Stable.V1) good in
  Expect_test_helpers_core.require_equal
    (module Entry)
    good
    (Binable.of_string (module Entry.Stable.V1) bin);
  [%expect
    {|
    ((idempotency_key k) (kind (Transfer transfer)) (code Transfer_intraledger)
     (reference "") (remittance_info "") (effective_at "2026-10-01 09:00:00Z")
     (recorded_at "2026-10-01 09:00:00Z") (business_date 2026-10-01) (sca ())
     (postings
      (((account bank:central) (side Debit) (amount "1.00 EUR"))
       ((account customer:alice) (side Credit) (amount "1.00 EUR")))))
    ("unbalanced entry: debits minus credits is not zero"
     (debits_minus_credits "-1.00 EUR")
     (idempotency_key      k))
    |}]
;;
