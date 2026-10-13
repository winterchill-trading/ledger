open! Core
open Ledger

let digest = String.make 32 'a'

let file values =
  Framing.header ~digest
  ^ String.concat (List.map values ~f:(Framing.encode_frame String.bin_writer_t))
;;

let show ?(accept = [ digest ]) contents =
  match Framing.decode ~accept String.bin_reader_t contents with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok { digest = _; records; valid_length; tail } ->
    print_s
      [%message (records : string list) (valid_length : int) (tail : Framing.Tail.t)]
;;

let%expect_test "clean file" =
  show (file [ "one"; "two"; "three" ]);
  [%expect {| ((records (one two three)) (valid_length 150) (tail Clean)) |}]
;;

let%expect_test "every truncation keeps the complete frames" =
  let contents = file [ "one"; "two" ] in
  List.iter [ 200; 112; 111; 84; 73; 41 ] ~f:(fun len ->
    let len = Int.min len (String.length contents) in
    show (String.prefix contents len));
  [%expect
    {|
    ((records (one two)) (valid_length 112) (tail Clean))
    ((records (one two)) (valid_length 112) (tail Clean))
    ((records (one)) (valid_length 76) (tail (Truncated (offset 76) (bytes 35))))
    ((records (one)) (valid_length 76) (tail (Truncated (offset 76) (bytes 8))))
    ((records ()) (valid_length 40) (tail (Truncated (offset 40) (bytes 33))))
    ((records ()) (valid_length 40) (tail (Truncated (offset 40) (bytes 1))))
    |}]
;;

let%expect_test "a flipped bit is corruption, not truncation" =
  let contents = Bytes.of_string (file [ "one"; "two"; "three" ]) in
  (* Frames are 32 bytes of overhead plus the payload ("one" is 4 bytes of bin_io), so the
     second frame starts at 76: flip a bit in its length (78) and in its payload (109). *)
  let flip pos =
    Bytes.set contents pos (Char.of_int_exn (Char.to_int (Bytes.get contents pos) lxor 1))
  in
  let copy = Bytes.copy contents in
  flip 109;
  show (Bytes.to_string contents);
  Bytes.blit ~src:copy ~src_pos:0 ~dst:contents ~dst_pos:0 ~len:(Bytes.length copy);
  flip 78;
  show (Bytes.to_string contents);
  [%expect
    {|
    ((records (one)) (valid_length 76)
     (tail (Corrupt (offset 76) (reason "checksum mismatch"))))
    ((records (one)) (valid_length 76)
     (tail (Corrupt (offset 76) (reason "damaged length header"))))
    |}]
;;

let%expect_test "bad length header in the middle" =
  let contents = Bytes.of_string (file [ "one"; "two"; "three" ]) in
  Stdlib.Bytes.set_int64_le contents 76 (-5L);
  Stdlib.Bytes.set_int64_le contents 84 4L;
  show (Bytes.to_string contents);
  [%expect
    {|
    ((records (one)) (valid_length 76)
     (tail (Corrupt (offset 76) (reason "bad length -5"))))
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
