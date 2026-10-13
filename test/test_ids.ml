open! Core
open Ledger

let%expect_test "valid and invalid ids" =
  List.iter
    [ "customer:42/eur"; ""; "has space"; String.make 129 'a'; "ok_id-1.2@x+y" ]
    ~f:(fun s ->
      print_s
        [%sexp
          (Account_id.of_string_or_error s |> Or_error.map ~f:Account_id.to_string
           : string Or_error.t)]);
  [%expect
    {|
    (Ok customer:42/eur)
    (Error ((Ledger.Account_id (id "")) "empty id"))
    (Error
     ((Ledger.Account_id (id "has space"))
      ("invalid character in id" (c " ") "has space")))
    (Error
     ((Ledger.Account_id
       (id
        aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa))
      ("id too long" (len 129) (max_length 128))))
    (Ok ok_id-1.2@x+y)
    |}]
;;

let%expect_test "pending sub-accounts" =
  let id = Account_id.of_string "customer:42/eur" in
  let p = Account_id.pending id in
  print_s
    [%sexp
      (p : Account_id.t)
      , (Account_id.is_pending p : bool)
      , (Account_id.is_pending id : bool)
      , (Account_id.parent_of_pending p : Account_id.t option)];
  [%expect {| (customer:42/eur/pending true false (customer:42/eur)) |}]
;;

let%expect_test "sexp of an invalid id raises" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    Hold_id.t_of_sexp (Sexp.Atom "a b"));
  [%expect {| ((Ledger.Hold_id (id "a b")) ("invalid character in id" (c " ") "a b")) |}]
;;

let%test_unit "ids round trip through bin_io and sexp" =
  Quickcheck.test Idempotency_key.quickcheck_generator ~f:(fun k ->
    let module S = Idempotency_key.Stable.V1 in
    [%test_result: Idempotency_key.t]
      (Binable.of_string (module S) (Binable.to_string (module S) k))
      ~expect:k;
    [%test_result: Idempotency_key.t] (S.t_of_sexp (S.sexp_of_t k)) ~expect:k)
;;
