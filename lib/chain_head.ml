open! Core

type t =
  { seq : int
  ; journal_length : int
  ; sha256 : string
  }
[@@deriving sexp, compare, equal]

let prefix = "ledger-head-v1"
let to_string t = sprintf "%s:%d:%d:%s" prefix t.seq t.journal_length t.sha256
let is_hex_digest s = String.length s = 64 && String.for_all s ~f:Char.is_hex_digit_lower

let of_string s =
  let error () = Or_error.error_s [%message "not a ledger chain head" s] in
  (* Plain decimal digits only: [Int.of_string] would also take "0x10" or "1_000". *)
  let decimal d =
    if (not (String.is_empty d)) && String.for_all d ~f:Char.is_digit
    then Int.of_string_opt d
    else None
  in
  match String.split s ~on:':' with
  | [ p; seq; len; sha256 ] when String.equal p prefix && is_hex_digest sha256 ->
    (match decimal seq, decimal len with
     | Some seq, Some journal_length when journal_length >= Framing.header_length ->
       Ok { seq; journal_length; sha256 }
     | _ -> error ())
  | _ -> error ()
;;

let of_snapshot (s : Snapshot.t) =
  { seq = s.seq
  ; journal_length = s.journal_length
  ; sha256 = Framing.to_hex s.journal_head
  }
;;

let read ?until path =
  let%map.Or_error last_seq, (summary : Framing.summary) =
    Framing.fold_file
      ?until
      ~accept:Record.compatible_digests
      Record.Stable.V1.bin_reader_t
      path
      ~init:0
      ~f:(fun _ (r : Record.t) -> Ok r.seq)
  in
  { seq = last_seq
  ; journal_length = summary.valid_length
  ; sha256 = Framing.to_hex summary.head
  }
;;

let of_file path = read path |> Or_error.tag_s ~tag:[%message "Chain_head.of_file" path]

let verify t ~journal =
  (let%bind.Or_error found = read ~until:t.journal_length journal in
   if equal found t
   then Ok ()
   else
     Or_error.error_s
       [%message
         "journal does not contain the published head" ~expected:(t : t) (found : t)])
  |> Or_error.tag_s ~tag:[%message "Chain_head.verify" journal]
;;
