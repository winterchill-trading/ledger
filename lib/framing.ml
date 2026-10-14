open! Core

let magic = "LEDGJRN1"
let digest_length = 32
let header_length = String.length magic + digest_length
let max_payload = 64 * 1024 * 1024

(* The length is stored twice, the second time bitwise complemented, so a damaged length
   in the middle of the file is reported as corruption instead of looking like a truncated
   last frame (which a repair would cut off, taking good frames with it). *)
let size_length = 16
let checksum_length = 16
let link_length = 32
let frame_overhead = size_length + checksum_length + link_length
let genesis = String.make link_length '\000'
let sha256 s = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s
let to_hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s

let header ~digest =
  if String.length digest <> digest_length
  then raise_s [%message "Framing.header: digest must be 32 hex characters" digest];
  magic ^ digest
;;

let encode_frame ~prev (writer : _ Bin_prot.Type_class.writer) v =
  if String.length prev <> link_length
  then raise_s [%message "Framing.encode_frame: prev must be a 32-byte hash"];
  let payload = Bigstring.to_string (Bin_prot.Utils.bin_dump writer v) in
  let len = String.length payload in
  let buf = Bytes.create (frame_overhead + len) in
  Stdlib.Bytes.set_int64_le buf 0 (Int64.of_int len);
  Stdlib.Bytes.set_int64_le buf 8 (Int64.lnot (Int64.of_int len));
  Bytes.From_string.blit
    ~src:(Md5.to_binary (Md5.digest_string payload))
    ~src_pos:0
    ~dst:buf
    ~dst_pos:size_length
    ~len:checksum_length;
  Bytes.From_string.blit
    ~src:prev
    ~src_pos:0
    ~dst:buf
    ~dst_pos:(size_length + checksum_length)
    ~len:link_length;
  Bytes.From_string.blit ~src:payload ~src_pos:0 ~dst:buf ~dst_pos:frame_overhead ~len;
  Bytes.to_string buf, sha256 payload
;;

module Tail = struct
  type t =
    | Clean
    | Truncated of
        { offset : int
        ; bytes : int
        }
    | Corrupt of
        { offset : int
        ; reason : string
        }
  [@@deriving sexp_of, compare, equal]
end

type 'a decoded =
  { digest : string
  ; records : 'a list
  ; valid_length : int
  ; head : string
  ; tail : Tail.t
  }

let decode_payload (reader : _ Bin_prot.Type_class.reader) payload =
  let buf = Bigstring.of_string payload in
  let pos_ref = ref 0 in
  match reader.read buf ~pos_ref with
  | v when !pos_ref = String.length payload -> Ok v
  | _ -> Error "payload has trailing bytes"
  | exception exn -> Error (sprintf "payload does not decode: %s" (Exn.to_string exn))
;;

type summary =
  { digest : string
  ; valid_length : int
  ; head : string
  ; tail : Tail.t
  }

(* Where frames come from: a string already in memory or a channel read frame by frame.
   [read n] returns [n] bytes, or fewer only at the end of the input. *)
type source =
  { length : int
  ; seek : int -> unit
  ; read : int -> string
  }

let source_of_string contents =
  let pos = ref 0 in
  { length = String.length contents
  ; seek = (fun p -> pos := p)
  ; read =
      (fun n ->
        let n = Int.min n (String.length contents - !pos) in
        let s = String.sub contents ~pos:!pos ~len:n in
        pos := !pos + n;
        s)
  }
;;

let source_of_channel ic =
  { length = Int64.to_int_exn (In_channel.length ic)
  ; seek = (fun p -> In_channel.seek ic (Int64.of_int p))
  ; read =
      (fun n ->
        let buf = Bytes.create n in
        let rec go off =
          if off = n
          then off
          else (
            match In_channel.input ic ~buf ~pos:off ~len:(n - off) with
            | 0 -> off
            | k -> go (off + k))
        in
        let got = go 0 in
        if got = n
        then Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
        else Bytes.To_string.sub buf ~pos:0 ~len:got)
  }
;;

let fold_source ?start ~accept reader source ~init ~f =
  let total = source.length in
  source.seek 0;
  let header = source.read header_length in
  if String.length header < header_length || not (String.is_prefix header ~prefix:magic)
  then
    Or_error.error_s
      [%message "not a ledger journal (missing or short header)" (total : int)]
  else (
    let digest = String.sub header ~pos:(String.length magic) ~len:digest_length in
    if not (List.mem accept digest ~equal:String.equal)
    then
      Or_error.error_s
        [%message
          "journal written with a record type this reader does not know"
            (digest : string)
            (accept : string list)]
    else (
      let rec loop offset prev acc =
        let finish tail =
          Ok (acc, { digest; valid_length = offset; head = prev; tail })
        in
        let fixed = source.read frame_overhead in
        let got = String.length fixed in
        if got = 0
        then finish Clean
        else if got < frame_overhead
        then finish (Truncated { offset; bytes = got })
        else (
          let len = Stdlib.String.get_int64_le fixed 0 in
          let check = Stdlib.String.get_int64_le fixed 8 in
          if not (Int64.equal check (Int64.lnot len))
          then finish (Corrupt { offset; reason = "damaged length header" })
          else if Int64.(len < 0L || len > of_int max_payload)
          then finish (Corrupt { offset; reason = sprintf "bad length %Ld" len })
          else (
            let len = Int64.to_int_exn len in
            (* Never allocate more than the file still holds, whatever the header says. *)
            let payload = source.read (Int.min len (total - offset - frame_overhead)) in
            if String.length payload < len
            then finish (Truncated { offset; bytes = got + String.length payload })
            else (
              let checksum = String.sub fixed ~pos:size_length ~len:checksum_length in
              let link =
                String.sub fixed ~pos:(size_length + checksum_length) ~len:link_length
              in
              if not (String.equal checksum (Md5.to_binary (Md5.digest_string payload)))
              then finish (Corrupt { offset; reason = "checksum mismatch" })
              else if not (String.equal link prev)
              then finish (Corrupt { offset; reason = "chain broken" })
              else (
                match decode_payload reader payload with
                | Error reason -> finish (Corrupt { offset; reason })
                | Ok v ->
                  (match f acc v with
                   | Error _ as e -> e
                   | Ok acc -> loop (offset + frame_overhead + len) (sha256 payload) acc)))))
      in
      let start, prev = Option.value start ~default:(header_length, genesis) in
      if start < header_length || start > total
      then
        Or_error.error_s
          [%message "start offset outside the file" (start : int) (total : int)]
      else (
        source.seek start;
        loop start prev init)))
;;

let fold ?start ~accept reader ic ~init ~f =
  fold_source ?start ~accept reader (source_of_channel ic) ~init ~f
;;

let fold_file ?start ~accept reader path ~init ~f =
  Or_error.try_with_join (fun () ->
    In_channel.with_file path ~binary:true ~f:(fun ic ->
      fold ?start ~accept reader ic ~init ~f))
;;

let decode ?start ~accept reader contents =
  let%map.Or_error records_rev, { digest; valid_length; head; tail } =
    fold_source
      ?start
      ~accept
      reader
      (source_of_string contents)
      ~init:[]
      ~f:(fun acc v -> Ok (v :: acc))
  in
  { digest; records = List.rev records_rev; valid_length; head; tail }
;;
