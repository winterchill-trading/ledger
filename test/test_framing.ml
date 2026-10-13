open! Core
open Ledger

let digest = String.make 32 'a'

let file values =
  let _, frames =
    List.fold_map values ~init:Framing.genesis ~f:(fun prev v ->
      let frame, hash = Framing.encode_frame ~prev String.bin_writer_t v in
      hash, frame)
  in
  Framing.header ~digest ^ String.concat frames
;;

let show ?(accept = [ digest ]) contents =
  match Framing.decode ~accept String.bin_reader_t contents with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok { digest = _; records; valid_length; head = _; tail } ->
    print_s
      [%message (records : string list) (valid_length : int) (tail : Framing.Tail.t)]
;;

let%expect_test "clean file" =
  show (file [ "one"; "two"; "three" ]);
  [%expect {| ((records (one two three)) (valid_length 246) (tail Clean)) |}]
;;

let%expect_test "every truncation keeps the complete frames" =
  let contents = file [ "one"; "two" ] in
  List.iter [ 300; 176; 175; 140; 105; 41 ] ~f:(fun len ->
    let len = Int.min len (String.length contents) in
    show (String.prefix contents len));
  [%expect
    {|
    ((records (one two)) (valid_length 176) (tail Clean))
    ((records (one two)) (valid_length 176) (tail Clean))
    ((records (one)) (valid_length 108)
     (tail (Truncated (offset 108) (bytes 67))))
    ((records (one)) (valid_length 108)
     (tail (Truncated (offset 108) (bytes 32))))
    ((records ()) (valid_length 40) (tail (Truncated (offset 40) (bytes 65))))
    ((records ()) (valid_length 40) (tail (Truncated (offset 40) (bytes 1))))
    |}]
;;

let%expect_test "a flipped bit is corruption, not truncation" =
  let contents = Bytes.of_string (file [ "one"; "two"; "three" ]) in
  (* Frames are 64 bytes of overhead plus the payload ("one" is 4 bytes of bin_io), so the
     second frame starts at 108: flip a bit in its length (110) and in its payload (173). *)
  let flip pos =
    Bytes.set contents pos (Char.of_int_exn (Char.to_int (Bytes.get contents pos) lxor 1))
  in
  let copy = Bytes.copy contents in
  flip 173;
  show (Bytes.to_string contents);
  Bytes.blit ~src:copy ~src_pos:0 ~dst:contents ~dst_pos:0 ~len:(Bytes.length copy);
  flip 110;
  show (Bytes.to_string contents);
  [%expect
    {|
    ((records (one)) (valid_length 108)
     (tail (Corrupt (offset 108) (reason "checksum mismatch"))))
    ((records (one)) (valid_length 108)
     (tail (Corrupt (offset 108) (reason "damaged length header"))))
    |}]
;;

let%expect_test "bad length header in the middle" =
  let contents = Bytes.of_string (file [ "one"; "two"; "three" ]) in
  Stdlib.Bytes.set_int64_le contents 108 (-5L);
  Stdlib.Bytes.set_int64_le contents 116 4L;
  show (Bytes.to_string contents);
  [%expect
    {|
    ((records (one)) (valid_length 108)
     (tail (Corrupt (offset 108) (reason "bad length -5"))))
    |}]
;;

let%expect_test "the hash chain catches a rewritten frame" =
  (* Replace the middle frame with a well-formed one (its own checksum is right), as
     someone editing the file would: the next frame's link no longer matches. *)
  let good = file [ "one"; "two"; "three" ] in
  let forged = file [ "one"; "TWO"; "three" ] in
  let spliced = String.prefix forged 176 ^ String.drop_prefix good 176 in
  show spliced;
  (* Without the last frame the forgery is undetectable from inside the file; the head
     hash, kept elsewhere, differs. *)
  let head contents =
    (Framing.decode ~accept:[ digest ] String.bin_reader_t contents |> Or_error.ok_exn)
      .head
  in
  print_s
    [%sexp
      (String.equal (head (String.prefix forged 176)) (head (String.prefix good 176))
       : bool)];
  [%expect
    {|
    ((records (one TWO)) (valid_length 176)
     (tail (Corrupt (offset 176) (reason "chain broken"))))
    false
    |}]
;;

let%expect_test "header problems are errors" =
  show "";
  show "LEDGJRN1abc";
  show ~accept:[ String.make 32 'b' ] (file [ "x" ]);
  [%expect
    {|
    ("not a ledger journal (missing or short header)" (total 0))
    ("not a ledger journal (missing or short header)" (total 11))
    ("journal written with a record type this reader does not know"
     (digest aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
     (accept (bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)))
    |}]
;;

let%test_unit "decode inverts encode" =
  Quickcheck.test
    (List.quickcheck_generator String.quickcheck_generator)
    ~sexp_of:[%sexp_of: string list]
    ~f:(fun values ->
      let contents = file values in
      let decoded =
        Framing.decode ~accept:[ digest ] String.bin_reader_t contents |> Or_error.ok_exn
      in
      [%test_result: string list] decoded.records ~expect:values;
      [%test_result: int] decoded.valid_length ~expect:(String.length contents))
;;
