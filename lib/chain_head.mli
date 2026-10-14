(** The head of a journal's hash chain, in a form to publish outside the system.

    Every frame of a journal carries the SHA-256 of the previous frame's payload
    ({!Framing}), so changing, removing or reordering any record breaks the chain. What
    the chain cannot show by itself is that records were cut off the end, or that the
    whole file was rewritten consistently. A head published elsewhere (sent to an auditor,
    a transparency log, a timestamping service, printed on a report) closes that gap: any
    later copy of the journal must still contain the published head, at the same sequence
    number and byte length, and {!verify} checks exactly that.

    {[
      let head = Journal.chain_head journal in
      publish (Chain_head.to_string head);
      (* later, possibly elsewhere *)
      Chain_head.of_string published
      |> Or_error.bind ~f:(Chain_head.verify ~journal:"bank.journal")
    ]} *)

open! Core

type t =
  { seq : int (** the last record covered; [0] for a journal with no records *)
  ; journal_length : int (** bytes of the journal covered, header included *)
  ; sha256 : string (** lowercase hex SHA-256 of the last covered frame's payload *)
  }
[@@deriving sexp, compare, equal]

(** One line without spaces: [ledger-head-v1:<seq>:<journal_length>:<sha256>]. *)
val to_string : t -> string

(** Parses {!to_string}'s format and nothing else. *)
val of_string : string -> t Or_error.t

(** The head a {!Snapshot} records. *)
val of_snapshot : Snapshot.t -> t

(** The head of the intact part of the journal file at [path], read frame by frame. *)
val of_file : string -> t Or_error.t

(** Checks that the journal at [journal] still contains [t]: a frame boundary at
    [journal_length], [seq] records before it and the same hash. Fails if the file was cut
    short of the head or anything up to it was changed; records appended after the head
    are fine. *)
val verify : t -> journal:string -> unit Or_error.t
