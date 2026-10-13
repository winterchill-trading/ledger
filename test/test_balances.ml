open! Core
open Ledger
open Helpers

let eur_ = Money.Currency.eur

let deposit ?(k = "dep-1") ?(amount = "100.00") ?(at = at 0) b account =
  post
    b
    (entry
       k
       ~at
       [ Posting.debit central_bank (eur amount); Posting.credit account (eur amount) ])
;;

let%expect_test "deposits and transfers" =
  let b = open_chart Balances.empty in
  let b = deposit b alice |> ok_exn in
  let b =
    post
      b
      (entry
         "pay-bob"
         [ Posting.debit alice (eur "30.00")
         ; Posting.credit bob (eur "29.50")
         ; Posting.credit fees (eur "0.50")
         ])
    |> ok_exn
  in
  List.iter [ alice; bob; fees; central_bank ] ~f:(fun a -> show_balance b a eur_);
  print_s [%sexp (Balances.next_seq b : int)];
  [%expect
    {|
    (customer:alice (available "70.00 EUR") (held "0.00 EUR")
     (posted "70.00 EUR"))
    (customer:bob (available "29.50 EUR") (held "0.00 EUR") (posted "29.50 EUR"))
    (income:fees (available "0.50 EUR") (held "0.00 EUR") (posted "0.50 EUR"))
    (bank:central (available "100.00 EUR") (held "0.00 EUR")
     (posted "100.00 EUR"))
    9
    |}]
;;

let%expect_test "customer accounts cannot go below zero" =
  let b = open_chart Balances.empty in
  let b = deposit b alice ~amount:"10.00" |> ok_exn in
  print_error
    (post
       b
       (entry
          "too-much"
          [ Posting.debit alice (eur "10.01"); Posting.credit bob (eur "10.01") ]));
  (* The settlement account may: it is flagged allow_negative. *)
  print_error
    (post
       b
       (entry
          "out"
          [ Posting.debit alice (eur "10.00"); Posting.credit network (eur "10.00") ]));
  print_error
    (post
       b
       (entry
          "in"
          [ Posting.debit network (eur "5.00"); Posting.credit bob (eur "5.00") ]));
  [%expect
    {|
    ("entry would take an account below zero" (id customer:alice)
     (balance_after "-0.01 EUR") (idempotency_key too-much))
    ok
    ok
    |}]
;;

let%expect_test "rule violations" =
  let b = open_chart Balances.empty in
  let b = deposit b alice |> ok_exn in
  (* Same key again. *)
  print_error (deposit b alice);
  (* Unknown account. *)
  print_error
    (post
       b
       (entry
          "x"
          [ Posting.debit alice (eur "1.00"); Posting.credit (id "nobody") (eur "1.00") ]));
  (* Ordinary entries may not touch a pending sub-account. *)
  print_error
    (post
       b
       (entry
          "y"
          [ Posting.debit alice (eur "1.00")
          ; Posting.credit (Account_id.pending alice) (eur "1.00")
          ]));
  (* Out of sequence. *)
  print_error
    (Balances.apply
       b
       { seq = 99; event = Account_opened (Account.create (id "z") Equity) });
  (* Account rules. *)
  print_error
    (Balances.apply
       b
       { seq = Balances.next_seq b
       ; event = Account_opened (Account.create alice Liability)
       });
  print_error
    (Balances.apply
       b
       { seq = Balances.next_seq b
       ; event = Account_opened (Account.create (Account_id.pending bob) Liability)
       });
  print_error
    (Balances.apply
       b
       { seq = Balances.next_seq b
       ; event = Account_opened (Account.create (id (String.make 125 'a')) Liability)
       });
  [%expect
    {|
    ("idempotency key already booked" (idempotency_key dep-1) (booked_at 7))
    ("posting to an account that is not open" (account nobody))
    ("only hold entries may post to a pending sub-account"
     (touches_pending
      (((account customer:alice/pending) (side Credit) (amount "1.00 EUR")))))
    ("out-of-sequence record" (seq 99) (expected 8))
    ("account already open" customer:alice)
    ("account ids ending in /pending are reserved" customer:bob/pending)
    ("account id too long for a pending sub-account"
     aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     (max_length 120))
    |}]
;;

let place b ?(k = "h1") ?(hold = "auth-1") amount =
  post
    b
    (Hold.place
       b
       ~hold_id:(Hold_id.of_string hold)
       ~account:alice
       ~amount:(eur amount)
       ~idempotency_key:(key k)
       ~effective_at:(at 1)
       ~recorded_at:(at 1)
       ())
;;

let%expect_test "card authorisation: hold, partial capture, release" =
  let b = open_chart Balances.empty in
  let b = deposit b alice |> ok_exn in
  let b = place b "40.00" |> ok_exn in
  show_balance b alice eur_;
  (* The merchant settles 35.00 of the 40.00 authorised. *)
  let b =
    post
      b
      (Hold.capture
         b
         ~hold_id:(Hold_id.of_string "auth-1")
         ~amount:(eur "35.00")
         ~to_:card_scheme
         ~idempotency_key:(key "c1")
         ~effective_at:(at 2)
         ~recorded_at:(at 2)
         ())
    |> ok_exn
  in
  show_balance b alice eur_;
  print_s [%sexp (Balances.open_holds b : Balances.Hold_state.t list)];
  let b =
    post
      b
      (Hold.release
         b
         ~hold_id:(Hold_id.of_string "auth-1")
         ~idempotency_key:(key "r1")
         ~effective_at:(at 3)
         ~recorded_at:(at 3)
         ())
    |> ok_exn
  in
  show_balance b alice eur_;
  print_s
    [%sexp
      (Balances.find_hold b (Hold_id.of_string "auth-1") : Balances.Hold_state.t option)];
  (* A closed hold cannot be released or captured again. *)
  print_error
    (post
       b
       (Hold.release
          b
          ~hold_id:(Hold_id.of_string "auth-1")
          ~amount:(eur "1.00")
          ~idempotency_key:(key "r2")
          ~effective_at:(at 3)
          ~recorded_at:(at 3)
          ()));
  [%expect
    {|
    (customer:alice (available "60.00 EUR") (held "40.00 EUR")
     (posted "100.00 EUR"))
    (customer:alice (available "60.00 EUR") (held "5.00 EUR")
     (posted "65.00 EUR"))
    (((id auth-1) (account customer:alice) (placed "40.00 EUR")
      (remaining "5.00 EUR") (placed_by h1) (expires_at ())))
    (customer:alice (available "65.00 EUR") (held "0.00 EUR")
     (posted "65.00 EUR"))
    (((id auth-1) (account customer:alice) (placed "40.00 EUR")
      (remaining "0.00 EUR") (placed_by h1) (expires_at ())))
    ("hold is already closed" (hold_id auth-1))
    |}]
;;

let%expect_test "holds cannot exceed available funds" =
  let b = open_chart Balances.empty in
  let b = deposit b alice ~amount:"50.00" |> ok_exn in
  let b = place b "30.00" |> ok_exn in
  print_error (place b ~k:"h2" ~hold:"auth-2" "20.01");
  print_error (place b ~k:"h2" ~hold:"auth-1" "1.00");
  (* Spending held funds is refused too: only 20.00 is available. *)
  print_error
    (post
       b
       (entry
          "spend"
          ~at:(at 2)
          [ Posting.debit alice (eur "25.00"); Posting.credit bob (eur "25.00") ]));
  (* Capturing more than the hold. *)
  print_error
    (post
       b
       (Hold.capture
          b
          ~hold_id:(Hold_id.of_string "auth-1")
          ~amount:(eur "30.01")
          ~to_:card_scheme
          ~idempotency_key:(key "c1")
          ~effective_at:(at 2)
          ~recorded_at:(at 2)
          ()));
  [%expect
    {|
    ("entry would take an account below zero" (id customer:alice)
     (balance_after "-0.01 EUR") (idempotency_key h2))
    ("hold id already used" (id auth-1))
    ("entry would take an account below zero" (id customer:alice)
     (balance_after "-5.00 EUR") (idempotency_key spend))
    ("more than the hold's remaining amount" (hold auth-1)
     (requested "30.01 EUR") (remaining "30.00 EUR"))
    |}]
;;

let%expect_test "hand-built hold entries must have the hold shape" =
  let b = open_chart Balances.empty in
  let b = deposit b alice |> ok_exn in
  let auth = Hold_id.of_string "auth-9" in
  (* Funds parked on someone else's pending account. *)
  print_error
    (post
       b
       (entry
          "h"
          ~kind:(Hold_place auth)
          [ Posting.debit alice (eur "5.00")
          ; Posting.credit (Account_id.pending bob) (eur "5.00")
          ]));
  (* A capture that sends the money back to the account itself. *)
  let b = place b ~hold:"auth-9" "5.00" |> ok_exn in
  print_error
    (post
       b
       (entry
          "c"
          ~at:(at 2)
          ~kind:(Hold_capture auth)
          [ Posting.debit (Account_id.pending alice) (eur "5.00")
          ; Posting.credit alice (eur "5.00")
          ]));
  [%expect
    {|
    ("a hold moves funds from an account to its own pending sub-account"
     (id auth-9))
    ("a capture moves funds out of the hold's pending sub-account to other accounts"
     (id auth-9))
    |}]
;;

let%expect_test "reversals" =
  let rev ?(at = at 5) b k reverses =
    post
      b
      (Reversal.create
         b
         ~reverses:(key reverses)
         ~idempotency_key:(key k)
         ~effective_at:at
         ~recorded_at:at
         ())
  in
  let b = open_chart Balances.empty in
  let b = deposit b alice |> ok_exn in
  let b = rev b "rev-1" "dep-1" |> ok_exn in
  show_balance b alice eur_;
  print_s [%sexp (Balances.reversed_by b (key "dep-1") : Idempotency_key.t option)];
  (* Twice, an unknown entry, and a reversal of a reversal are refused. *)
  print_error (rev b "rev-2" "dep-1");
  print_error (rev b "rev-3" "nothing");
  print_error (rev b "rev-4" "rev-1");
  (* A reversal must mirror the original exactly. *)
  let b = deposit b alice ~k:"dep-2" ~at:(at 6) |> ok_exn in
  print_error
    (post
       b
       (entry
          "bad-rev"
          ~at:(at 6)
          ~kind:(Reversal { reverses = key "dep-2" })
          [ Posting.debit alice (eur "50.00"); Posting.credit central_bank (eur "50.00") ]));
  (* Reversing a deposit already spent would overdraw the customer. *)
  let b =
    post
      b
      (entry
         "spend"
         ~at:(at 7)
         [ Posting.debit alice (eur "80.00"); Posting.credit bob (eur "80.00") ])
    |> ok_exn
  in
  print_error (rev b "rev-5" "dep-2" ~at:(at 8));
  [%expect
    {|
    (customer:alice (available "0.00 EUR") (held "0.00 EUR") (posted "0.00 EUR"))
    (rev-1)
    ("entry already reversed" (reverses dep-1) (by rev-1))
    ("no entry booked under this key" (reverses nothing))
    ("only transfers can be reversed; release or capture a hold instead"
     (reverses rev-1) (original.kind (Reversal (reverses dep-1))))
    ("a reversal must post the original's postings on the opposite sides"
     (reverses dep-2))
    ("entry would take an account below zero" (id customer:alice)
     (balance_after "-80.00 EUR") (idempotency_key rev-5))
    |}]
;;
