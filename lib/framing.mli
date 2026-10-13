(** The byte layout of a journal file, independent of what the records are.

    {v
    header : "LEDGJRN1" (8 bytes) ++ hex bin_shape digest of the record type (32 bytes)
    frame* : payload length, 8-byte little-endian
             ++ the same length bitwise complemented (8 bytes)
             ++ MD5 of the payload (16 bytes)
             ++ payload (bin_io of one record)
    v}

    Files are only appended to. A crash in the middle of an append leaves a partial last
    frame ({!Tail.Truncated}); any other damage (a flipped bit, a frame from another file
    spliced in, a damaged length header) shows up as {!Tail.Corrupt} because the length
    check, the checksum or the decoding fails. So a repair that cuts a truncated tail
    never cuts intact frames after a damaged one.

    The checksum is MD5 because Core ships it and it only has to catch accidents, not an
    attacker who can rewrite the file anyway. *)

open! Core

(** ["LEDGJRN1"]. *)
val magic : string

(** [magic] plus the 32-character digest. *)
val header_length : int

(** Payloads above this (64 MiB) are treated as corruption: no single journal record is
    anywhere near it, so such a length header must be damaged. *)
val max_payload : int

(** Raises unless [digest] is 32 characters. *)
val header : digest:string -> string

val encode_frame : 'a Bin_prot.Type_class.writer -> 'a -> string

module Tail : sig
  type t =
    | Clean
    | Truncated of
        { offset : int
        ; bytes : int
        } (** a partial frame of [bytes] bytes starts at [offset] *)
    | Corrupt of
        { offset : int
        ; reason : string
        } (** the frame starting at [offset] is damaged *)
  [@@deriving sexp_of, compare, equal]
end

type 'a decoded =
  { digest : string (** from the header *)
  ; records : 'a list (** in file order, up to the first damaged frame *)
  ; valid_length : int (** bytes of header and complete, intact frames *)
  ; tail : Tail.t
  }

(** Decodes a whole file. Errors (rather than returning records) only when the header is
    missing, short, or names a digest not in [accept]. *)
val decode
  :  accept:string list
  -> 'a Bin_prot.Type_class.reader
  -> string
  -> 'a decoded Or_error.t
