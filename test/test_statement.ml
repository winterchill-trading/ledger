open! Core
open Ledger
open Helpers

(* A month of activity on Alice's account, as records. *)
let records =
  let b = ref Balances.empty in
  let records = ref [] in
  let add event =
    let r = { Record.seq = Balances.next_seq !b; event } in
    b := Balances.apply !b r |> ok_exn;
    records := r :: !records
  in
  List.iter chart ~f:(fun a -> add (Account_opened a));
  let post ?kind ?reference k minutes postings =
    add (Posted (entry ?kind ?reference ~at:(at minutes) k postings |> ok_exn))
  in
  post
    "dep-1"
    0
    ~reference:"sepa-in-001"
    [ Posting.debit central_bank (eur "1500.00"); Posting.credit alice (eur "1500.00") ];
  post
    "rent"
    60
    ~reference:"so-rent-10"
    [ Posting.debit alice (eur "850.00"); Posting.credit network (eur "850.00") ];
  add
    (Posted
       (Hold.place
          !b
          ~hold_id:(Hold_id.of_string "auth-77")
          ~account:alice
          ~amount:(eur "42.10")
          ~idempotency_key:(key "auth-77")
          ~reference:"card-coffee"
          ~effective_at:(at 120)
          ~recorded_at:(at 120)
          ()
        |> ok_exn));
  add
    (Posted
       (Hold.capture
          !b
          ~hold_id:(Hold_id.of_string "auth-77")
          ~amount:(eur "39.90")
          ~to_:card_scheme
          ~idempotency_key:(key "cap-77")
          ~reference:"card-coffee"
          ~effective_at:(at 1500)
          ~recorded_at:(at 1500)
          ()
        |> ok_exn));
  add
    (Posted
       (Hold.release
          !b
          ~hold_id:(Hold_id.of_string "auth-77")
          ~idempotency_key:(key "rel-77")
          ~reference:"card-coffee"
          ~effective_at:(at 1500)
          ~recorded_at:(at 1500)
          ()
        |> ok_exn));
  post
    "fee-1"
    2000
    ~kind:(Transfer "fee")
    ~reference:"monthly-fee"
    [ Posting.debit alice (eur "2.50"); Posting.credit fees (eur "2.50") ];
  let fee_seq = Balances.next_seq !b - 1 in
  add
    (Posted
       (Reversal.create
          !b
          ~seq:fee_seq
          ~idempotency_key:(key "fee-1-waived")
          ~effective_at:(at 2100)
          ~recorded_at:(at 2100)
          ()
        |> ok_exn));
  List.rev !records
;;

let print ?from ?until ?after_seq ?limit account =
  Statement.create
    ?from
    ?until
    ?after_seq
    ?limit
    records
    ~account
    ~currency:Money.Currency.eur
  |> ok_exn
  |> Statement.to_string_hum
  |> print_string
;;

let%expect_test "statement" =
  print alice;
  [%expect
    {|
    Statement customer:alice EUR
      SEQ  EFFECTIVE (UTC)      KIND               REFERENCE               DEBIT         CREDIT        BALANCE
                                opening balance                                                           0.00
        7  2026-10-01 09:00:00  transfer           sepa-in-001                          1500.00        1500.00
        8  2026-10-01 10:00:00  transfer           so-rent-10             850.00                        650.00
        9  2026-10-01 11:00:00  hold auth-77       card-coffee             42.10                        607.90
       11  2026-10-02 10:00:00  release auth-77    card-coffee                             2.20         610.10
       12  2026-10-02 18:20:00  fee                monthly-fee              2.50                        607.60
       13  2026-10-02 20:00:00  reversal of #12    monthly-fee                             2.50         610.10
                                closing balance                                                         610.10
    |}]
;;

let%expect_test "the pending sub-account shows the hold's life" =
  print (Account_id.pending alice);
  [%expect
    {|
    Statement customer:alice/pending EUR
      SEQ  EFFECTIVE (UTC)      KIND               REFERENCE               DEBIT         CREDIT        BALANCE
                                opening balance                                                           0.00
        9  2026-10-01 11:00:00  hold auth-77       card-coffee                            42.10          42.10
       10  2026-10-02 10:00:00  capture auth-77    card-coffee             39.90                          2.20
       11  2026-10-02 10:00:00  release auth-77    card-coffee              2.20                          0.00
                                closing balance                                                           0.00
    |}]
;;

let%expect_test "time range" =
  (* From the second day: the opening balance counts everything before. *)
  print alice ~from:(at 1440) ~until:(at 2050);
  [%expect
    {|
    Statement customer:alice EUR
      SEQ  EFFECTIVE (UTC)      KIND               REFERENCE               DEBIT         CREDIT        BALANCE
                                opening balance                                                         607.90
       11  2026-10-02 10:00:00  release auth-77    card-coffee                             2.20         610.10
       12  2026-10-02 18:20:00  fee                monthly-fee              2.50                        607.60
                                closing balance                                                         607.60
    |}]
;;

let%expect_test "pages never split an entry and never skip a line" =
  let rec pages after_seq =
    let s =
      Statement.create
        records
        ~account:alice
        ~currency:Money.Currency.eur
        ?after_seq
        ~limit:2
      |> ok_exn
    in
    print_s
      [%message
        ""
          ~seqs:(List.map s.lines ~f:(fun l -> l.seq) : int list)
          ~opening:(Money.Amount.to_string s.opening)
          ~closing:(Money.Amount.to_string s.closing)
          ~next:(s.next_after_seq : int option)];
    Option.iter s.next_after_seq ~f:(fun seq -> pages (Some seq))
  in
  pages None;
  [%expect
    {|
    ((seqs (7 8)) (opening "0.00 EUR") (closing "650.00 EUR") (next (8)))
    ((seqs (9 11)) (opening "650.00 EUR") (closing "610.10 EUR") (next (11)))
    ((seqs (12 13)) (opening "610.10 EUR") (closing "610.10 EUR") (next ()))
    |}]
;;

let%expect_test "unknown account" =
  print_error
    (Statement.create records ~account:(id "customer:carol") ~currency:Money.Currency.eur);
  [%expect {| ("account not opened" (account customer:carol)) |}]
;;
