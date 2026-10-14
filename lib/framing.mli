(** The byte layout of a journal file, independent of what the records are.

    {v
    header : "LEDGJRN1" (8 bytes) ++ hex bin_shape digest of the record type (32 bytes)
    frame* : payload length, 8-byte little-endian
             ++ the same length bitwise complemented (8 bytes)
             ++ MD5 of the payload (16 bytes)
             ++ SHA-256 of the previous frame's payload (32 bytes; zeros for the first)
             ++ payload (bin_io of one record)
    v}

    The SHA-256 links make the file a hash chain: changing, removing or reordering any
    frame breaks the link of the frame after it ([Corrupt] with reason ["chain broken"]),
    even if whoever changed it also fixed the MD5. Only the last frame is not covered by a
    later link; {!decoded.head}, its hash, is the value to keep somewhere else (snapshots
    record it) to cover that too.

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

(** The [prev] of the first frame: 32 zero bytes. *)
val genesis : string

(** [encode_frame ~prev writer v] is the frame and the SHA-256 of its payload (the [prev]
    of the next frame). *)
val encode_frame : prev:string -> 'a Bin_prot.Type_class.writer -> 'a -> string * string

(** Lowercase hex, for printing hashes. *)
val to_hex : string -> string

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
  ; head : string (** SHA-256 of the last intact frame's payload, or [prev] if none *)
  ; tail : Tail.t
  }

(** What a decoder found besides the records: the fields of {!decoded} other than
    [records]. *)
type summary =
  { digest : string
  ; valid_length : int
  ; head : string
  ; tail : Tail.t
  }

(** [fold ~accept reader ic ~init ~f] decodes frames from [ic] one at a time, from the
    start of the channel, and folds [f] over the records, so memory does not grow with the
    file. It stops at the first damaged or partial frame like {!decode} and reports it in
    the summary. An [Error] from [f] stops the fold and is returned as is.

    [ic] must be seekable (a file); it is left positioned after the last frame read. *)
val fold
  :  ?start:int * string (** as for {!decode} *)
  -> accept:string list
  -> 'a Bin_prot.Type_class.reader
  -> In_channel.t
  -> init:'acc
  -> f:('acc -> 'a -> 'acc Or_error.t)
  -> ('acc * summary) Or_error.t

(** {!fold} over the file at [path]. Errors opening or reading the file are returned, not
    raised. *)
val fold_file
  :  ?start:int * string
  -> accept:string list
  -> 'a Bin_prot.Type_class.reader
  -> string
  -> init:'acc
  -> f:('acc -> 'a -> 'acc Or_error.t)
  -> ('acc * summary) Or_error.t

(** Decodes a whole file already read into memory. Errors (rather than returning records)
    only when the header is missing, short, or names a digest not in [accept]. *)
val decode
  :  ?start:int * string
       (** offset of a frame boundary to start at, after a part already decoded, and the
           hash of the frame before it (default: just after the header, {!genesis}) *)
  -> accept:string list
  -> 'a Bin_prot.Type_class.reader
  -> string
  -> 'a decoded Or_error.t
