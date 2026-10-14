open! Core
open Ledger

(* [data/digest-v1.journal] was written by [ledger demo] when {!Record.digest} was
   132f0e07...; it must stay readable after any compatible change to the record type (see
   record.mli). Never regenerate it. *)
let%expect_test "a journal from the first record digest still reads and replays" =
  let path = "data/digest-v1.journal" in
  let records, tail = Journal.read path |> Or_error.ok_exn in
  let balances = Journal.replay path |> Or_error.ok_exn in
  print_s [%message "" ~records:(List.length records : int) (tail : Framing.Tail.t)];
  print_string (Trial_balance.to_string_hum (Trial_balance.of_balances balances));
  [%expect
    {|
    ((records 8) (tail Clean))
    ACCOUNT         CURRENCY                DEBIT               CREDIT
    bank:central    EUR                    250.00
    customer:alice  EUR                                         230.10
    external:cards  EUR                                          18.40
    income:fees     EUR                                           1.50
    TOTAL           EUR                    250.00               250.00
    |}]
;;
