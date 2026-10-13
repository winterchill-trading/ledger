(** End-of-day snapshots: the ledger state after a {!Record.Event.Day_closed} record, with
    the day's balance rows, tied to the exact journal bytes it covers.

    A snapshot is derived data. It lets {!Journal.open_} skip re-folding the history it
    covers, and it is checked rather than trusted: the journal prefix it names must have
    the recorded MD5, and {!verify} replays that prefix from scratch and requires the same
    state and rows, byte for byte.

    The file uses the journal's {!Framing} layout with one frame and its own digest. *)

open! Core

type t =
  { business_date : Date.t
  ; seq : int (** the [Day_closed] record; everything up to it is covered *)
  ; journal_length : int (** bytes of the journal covered, header included *)
  ; journal_md5 : string (** hex MD5 of those bytes *)
  ; journal_head : string
  (** SHA-256 of the last covered frame's payload: the hash-chain head, the value to keep
      outside the system (sent to an auditor, printed on a report) as the anchor *)
  ; rows : Balances.Day_row.t list
  ; state : Balances.t
  }
[@@deriving sexp_of]

(** Hex [bin_shape] digest of the snapshot type, in the file header. *)
val digest : string

val write : t -> string -> unit Or_error.t
val read : string -> t Or_error.t

(** [<journal path>.snapshots/<YYYY-MM-DD>.snap] *)
val default_path : journal:string -> Date.t -> string

(** Checks the snapshot against the journal file at [journal]: the covered prefix has the
    recorded length and MD5, and replaying it from an empty state gives the same state and
    rows. *)
val verify : t -> journal:string -> unit Or_error.t
