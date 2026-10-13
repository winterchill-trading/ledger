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
let frame_overhead = size_length + checksum_length

let header ~digest =
  if String.length digest <> digest_length
  then raise_s [%message "Framing.header: digest must be 32 hex characters" digest];
  magic ^ digest
;;

let encode_frame (writer : _ Bin_prot.Type_class.writer) v =
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
  Bytes.From_string.blit ~src:payload ~src_pos:0 ~dst:buf ~dst_pos:frame_overhead ~len;
  Bytes.to_string buf
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

let decode ~accept reader contents =
  let total = String.length contents in
  if total < header_length || not (String.is_prefix contents ~prefix:magic)
  then
    Or_error.error_s
      [%message "not a ledger journal (missing or short header)" (total : int)]
  else (
    let digest = String.sub contents ~pos:(String.length magic) ~len:digest_length in
    if not (List.mem accept digest ~equal:String.equal)
    then
      Or_error.error_s
        [%message
          "journal written with a record type this reader does not know"
            (digest : string)
            (accept : string list)]
    else (
      let rec loop offset acc =
        let remaining = total - offset in
        let finish tail =
          Ok { digest; records = List.rev acc; valid_length = offset; tail }
        in
        if remaining = 0
        then finish Clean
        else if remaining < frame_overhead
        then finish (Truncated { offset; bytes = remaining })
        else (
          let len = Stdlib.String.get_int64_le contents offset in
          let check = Stdlib.String.get_int64_le contents (offset + 8) in
          if not (Int64.equal check (Int64.lnot len))
          then finish (Corrupt { offset; reason = "damaged length header" })
          else if Int64.(len < 0L || len > of_int max_payload)
          then finish (Corrupt { offset; reason = sprintf "bad length %Ld" len })
          else (
            let len = Int64.to_int_exn len in
            if remaining < frame_overhead + len
            then finish (Truncated { offset; bytes = remaining })
            else (
              let checksum =
                String.sub contents ~pos:(offset + size_length) ~len:checksum_length
              in
              let payload = String.sub contents ~pos:(offset + frame_overhead) ~len in
              if not (String.equal checksum (Md5.to_binary (Md5.digest_string payload)))
              then finish (Corrupt { offset; reason = "checksum mismatch" })
              else (
                match decode_payload reader payload with
                | Error reason -> finish (Corrupt { offset; reason })
                | Ok v -> loop (offset + frame_overhead + len) (v :: acc)))))
      in
      loop header_length []))
;;
