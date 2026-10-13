open! Core
open Ledger
open Helpers
module Time_source = Async_kernel.Time_source

let with_dir f =
  let dir = Filename_unix.temp_dir "ledger" "test" in
  Exn.protect
    ~f:(fun () -> f dir)
    ~finally:(fun () ->
      ignore
        (Core_unix.system (sprintf "rm -rf %s" (Filename.quote dir))
         : Core_unix.Exit_or_signal.t))
;;

let clock () = Time_source.create ~now:(at 0) ()
let show_result r = print_s [%sexp (r : Journal.Append_result.t Or_error.t)]

let seq_of r =
  print_s [%sexp (Or_error.map r ~f:Journal.Append_result.seq : int Or_error.t)]
;;

(* Opens the chart, books a deposit and places a hold. *)
let populate j =
  List.iter chart ~f:(fun a ->
    ignore (Journal.open_account j a |> ok_exn : Journal.Append_result.t));
  ignore
    (Journal.transfer
       j
       ~idempotency_key:(key "dep-1")
       ~tag:"deposit"
       ~reference:"sepa-e2e-1"
       ~effective_at:(at 0)
       [ Posting.debit central_bank (eur "100.00"); Posting.credit alice (eur "100.00") ]
     |> ok_exn
     : Journal.Append_result.t);
  ignore
    (Journal.hold
       j
       ~hold_id:(Hold_id.of_string "auth-1")
       ~account:alice
       ~amount:(eur "25.00")
       ~idempotency_key:(key "h1")
       ~effective_at:(at 1)
       ()
     |> ok_exn
     : Journal.Append_result.t)
;;

let%expect_test "the record digest is pinned" =
  print_endline Record.digest;
  print_s [%sexp (Record.compatible_digests : string list)];
  (* New journals must be readable by this code. *)
  assert (List.mem Record.compatible_digests Record.digest ~equal:String.equal);
  [%expect
    {|
    3d66d278d2cb87148044dbc25f542224
    (3d66d278d2cb87148044dbc25f542224)
    |}]
;;

let%expect_test "create, append, reopen, replay" =
  with_dir (fun dir ->
    let path = dir ^/ "j.bin" in
    let j = Journal.create ~time_source:(clock ()) path |> ok_exn in
    populate j;
    let before = Journal.balances j in
    Journal.close j;
    Journal.close j;
    let j = Journal.open_ path |> ok_exn in
    let replayed = Journal.replay path |> ok_exn in
    let same b =
      Trial_balance.equal (Trial_balance.of_balances b) (Trial_balance.of_balances before)
    in
    print_s [%message (same (Journal.balances j) : bool) (same replayed : bool)];
    show_balance (Journal.balances j) alice Money.Currency.eur;
    print_s [%sexp (List.length (Journal.records j) : int)];
    (* The creating journal's clock stamped recorded_at; it survives the reopen. *)
    print_s [%sexp (List.last_exn (Journal.records j) : Record.t)];
    (* Appends after the reopen continue the sequence. *)
    seq_of
      (Journal.release
         j
         ~hold_id:(Hold_id.of_string "auth-1")
         ~idempotency_key:(key "r1")
         ~effective_at:(at 2)
         ());
    Journal.close j;
    print_error
      ~dir
      (Journal.release
         j
         ~hold_id:(Hold_id.of_string "auth-1")
         ~idempotency_key:(key "r2")
         ~effective_at:(at 2)
         ());
    print_error ~dir (Journal.create path));
  [%expect
    {|
    (("same (Journal.balances j)" true) ("same replayed" true))
    (customer:alice (available "75.00 EUR") (held "25.00 EUR")
     (posted "100.00 EUR"))
    8
    ((seq 8)
     (event
      (Posted
       ((idempotency_key h1) (kind (Hold_place auth-1)) (reference "")
        (effective_at "2026-10-01 09:01:00Z")
        (recorded_at "2026-10-01 09:00:00Z")
        (postings
         (((account customer:alice) (side Debit) (amount "25.00 EUR"))
          ((account customer:alice/pending) (side Credit) (amount "25.00 EUR"))))))))
    (Ok 9)
    ("journal is closed" $DIR/j.bin)
    (Unix.Unix_error "File exists" open
     "((filename $DIR/j.bin) (mode (O_WRONLY O_CREAT O_EXCL O_APPEND)) (perm 0o644))")
    |}]
;;

let%expect_test "retries are duplicates, reused keys are errors" =
  with_dir (fun dir ->
    let j = Journal.create ~fsync:false (dir ^/ "j.bin") |> ok_exn in
    populate j;
    let deposit amount =
      Journal.transfer
        j
        ~idempotency_key:(key "dep-1")
        ~tag:"deposit"
        ~reference:"sepa-e2e-1"
        ~effective_at:(at 0)
        [ Posting.debit central_bank (eur amount); Posting.credit alice (eur amount) ]
    in
    show_result (deposit "100.00");
    show_result (deposit "100.01");
    (* Opening an account twice: identical is a duplicate, different is an error. *)
    show_result (Journal.open_account j (List.hd_exn chart));
    show_result (Journal.open_account j (Account.create central_bank Equity));
    (* Releasing the same hold twice with one key: the second call cannot build an entry
       (nothing remains) but is still recognised as the same request. *)
    let release k =
      Journal.release
        j
        ~hold_id:(Hold_id.of_string "auth-1")
        ~idempotency_key:(key k)
        ~effective_at:(at 2)
        ()
    in
    seq_of (release "r1");
    seq_of (release "r1");
    seq_of (release "r-other");
    Journal.close j);
  [%expect
    {|
    (Ok (Duplicate (seq 7)))
    (Error
     ("idempotency key already used for a different entry"
      (idempotency_key dep-1) (booked_at 7)))
    (Ok (Duplicate (seq 1)))
    (Error ("account already open" bank:central))
    (Ok 9)
    (Ok 9)
    (Error ("hold is already closed" (hold_id auth-1)))
    |}]
;;

let%expect_test "a crash mid-append is repaired only on request" =
  with_dir (fun dir ->
    let path = dir ^/ "j.bin" in
    let j = Journal.create ~fsync:false path |> ok_exn in
    populate j;
    Journal.close j;
    let full = In_channel.read_all path in
    (* Cut the last frame in half, as a crash during its write would. *)
    Out_channel.write_all path ~data:(String.drop_suffix full 10);
    print_error ~dir (Journal.open_ path);
    print_s [%sexp (Journal.read path |> Or_error.map ~f:snd : Framing.Tail.t Or_error.t)];
    let j = Journal.open_ ~repair_truncated_tail:true path |> ok_exn in
    print_s [%sexp (Balances.next_seq (Journal.balances j) : int)];
    (* The lost hold can now be placed again under the same key. *)
    seq_of
      (Journal.hold
         j
         ~hold_id:(Hold_id.of_string "auth-1")
         ~account:alice
         ~amount:(eur "25.00")
         ~idempotency_key:(key "h1")
         ~effective_at:(at 1)
         ());
    Journal.close j;
    print_s
      [%sexp (In_channel.read_all path |> String.length = String.length full : bool)]);
  [%expect
    {|
    ((Journal.open_ $DIR/j.bin)
     ("journal ends in a partial record (a crash during an append); reopen with repair_truncated_tail to cut it off"
      (tail (Truncated (offset 501) (bytes 113)))))
    (Ok (Truncated (offset 501) (bytes 113)))
    8
    (Ok 8)
    true
    |}]
;;

let%expect_test "corruption is detected on reopen" =
  with_dir (fun dir ->
    let path = dir ^/ "j.bin" in
    let j = Journal.create ~fsync:false path |> ok_exn in
    populate j;
    Journal.close j;
    let full = Bytes.of_string (In_channel.read_all path) in
    (* Flip one bit in the middle of the file. *)
    let pos = Bytes.length full / 2 in
    Bytes.set full pos (Char.of_int_exn (Char.to_int (Bytes.get full pos) lxor 0x10));
    Out_channel.write_all path ~data:(Bytes.to_string full);
    print_error ~dir (Journal.open_ ~repair_truncated_tail:true path);
    print_error ~dir (Journal.replay path);
    (* A file that is not a journal at all. *)
    Out_channel.write_all path ~data:"hello";
    print_error ~dir (Journal.open_ path));
  [%expect
    {|
    ((Journal.open_ $DIR/j.bin)
     ("corrupt journal"
      (tail (Corrupt (offset 309) (reason "damaged length header")))))
    ((Journal.replay $DIR/j.bin)
     ("corrupt journal"
      (tail (Corrupt (offset 309) (reason "damaged length header")))))
    ((Journal.open_ $DIR/j.bin)
     ("not a ledger journal (missing or short header)" (total 5)))
    |}]
;;

let%expect_test "a journal from another record type is refused" =
  with_dir (fun dir ->
    let path = dir ^/ "j.bin" in
    Out_channel.write_all path ~data:(Framing.header ~digest:(String.make 32 '0'));
    print_error ~dir (Journal.open_ path));
  [%expect
    {|
    ((Journal.open_ $DIR/j.bin)
     ("journal written with a record type this reader does not know"
      (digest 00000000000000000000000000000000)
      (accept (3d66d278d2cb87148044dbc25f542224))))
    |}]
;;

let%expect_test "one writer at a time" =
  with_dir (fun dir ->
    let path = dir ^/ "j.bin" in
    let j = Journal.create ~fsync:false path |> ok_exn in
    (* lockf locks belong to the process, so a second open from this process would
       succeed; check from a child process instead. *)
    (match Core_unix.fork () with
     | `In_the_child ->
       let code = if Result.is_error (Journal.open_ path) then 0 else 1 in
       Core_unix.exit_immediately code
     | `In_the_parent pid ->
       print_s [%sexp (Core_unix.waitpid pid : Core_unix.Exit_or_signal.t)]);
    Journal.close j;
    match Core_unix.fork () with
    | `In_the_child ->
      let code = if Result.is_ok (Journal.open_ path) then 0 else 1 in
      Core_unix.exit_immediately code
    | `In_the_parent pid ->
      print_s [%sexp (Core_unix.waitpid pid : Core_unix.Exit_or_signal.t)]);
  [%expect {|
    (Ok ())
    (Ok ())
    |}]
;;
