open! Core
open Ledger
open Helpers
open Test_journal

let%expect_test "a published head survives appends and catches truncation" =
  with_dir (fun dir ->
    let path = dir ^/ "j.bin" in
    let j = Journal.create ~time_source:(clock ()) ~fsync:false path |> ok_exn in
    populate j;
    let head = Journal.chain_head j in
    let published = Chain_head.to_string head in
    print_endline published;
    print_s
      [%message
        ""
          ~round_trip:
            (Chain_head.equal (Chain_head.of_string published |> ok_exn) head : bool)
          ~of_file:(Chain_head.equal (Chain_head.of_file path |> ok_exn) head : bool)];
    (* More records after the head: still contains it. *)
    seq_of
      (Journal.transfer
         j
         ~idempotency_key:(key "dep-2")
         ~effective_at:(at 2)
         [ Posting.debit central_bank (eur "1.00"); Posting.credit bob (eur "1.00") ]);
    print_error (Chain_head.verify head ~journal:path);
    let snapshot = Journal.close_day j |> ok_exn in
    print_s
      [%message
        ""
          ~snapshot_head_is_journal_head:
            (Chain_head.equal (Chain_head.of_snapshot snapshot) (Journal.chain_head j)
             : bool)];
    Journal.close j;
    (* Cut the file back to just before the head's last frame. *)
    Core_unix.truncate path ~len:(Int64.of_int (head.journal_length - 1));
    print_error ~dir (Chain_head.verify head ~journal:path);
    [%expect
      {|
      ledger-head-v1:8:902:17f66f38d4e41ddaf05c570a7656d4416594f99e57f37dfc3dbb6974aada0a74
      ((round_trip true) (of_file true))
      (Ok 9)
      ok
      (snapshot_head_is_journal_head true)
      ((Chain_head.verify $DIR/j.bin)
       ("journal does not contain the published head"
        (expected
         ((seq 8) (journal_length 902)
          (sha256 17f66f38d4e41ddaf05c570a7656d4416594f99e57f37dfc3dbb6974aada0a74)))
        (found
         ((seq 7) (journal_length 739)
          (sha256 6b26b1ddfb8e9d55c767c752caa2e5d306a4d50a96b5763d735b6d5e5f46b43c)))))
      |}])
;;

let%expect_test "a rewritten journal of the same length does not contain the head" =
  with_dir (fun dir ->
    let make name ~amount =
      let path = dir ^/ name in
      let j = Journal.create ~time_source:(clock ()) ~fsync:false path |> ok_exn in
      List.iter chart ~f:(fun a ->
        ignore (Journal.open_account j a |> ok_exn : Journal.Append_result.t));
      ignore
        (Journal.transfer
           j
           ~idempotency_key:(key "dep-1")
           ~effective_at:(at 0)
           [ Posting.debit central_bank (eur amount); Posting.credit alice (eur amount) ]
         |> ok_exn
         : Journal.Append_result.t);
      let head = Journal.chain_head j in
      Journal.close j;
      path, head
    in
    let _, head = make "a.bin" ~amount:"100.00" in
    let forged, forged_head = make "b.bin" ~amount:"900.00" in
    print_s
      [%message
        ""
          ~same_length:(head.journal_length = forged_head.journal_length : bool)
          ~verifies:(Result.is_ok (Chain_head.verify head ~journal:forged) : bool)];
    [%expect {| ((same_length true) (verifies false)) |}])
;;

let%expect_test "of_string takes only its own format" =
  let good = "ledger-head-v1:3:500:" ^ String.make 64 'a' in
  List.iter
    [ good
    ; "ledger-head-v2:3:500:" ^ String.make 64 'a'
    ; "ledger-head-v1:0x3:500:" ^ String.make 64 'a'
    ; "ledger-head-v1:3:500:" ^ String.make 64 'A'
    ; "ledger-head-v1:3:500:" ^ String.make 63 'a'
    ; "ledger-head-v1:3:10:" ^ String.make 64 'a'
    ; "ledger-head-v1:-3:500:" ^ String.make 64 'a'
    ]
    ~f:(fun s -> print_s [%sexp (Result.is_ok (Chain_head.of_string s) : bool)]);
  [%expect {|
    true
    false
    false
    false
    false
    false
    false
    |}]
;;
