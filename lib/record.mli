(** One record of the journal, numbered from 1 with no gaps. *)

open! Core

module Event : sig
  type t =
    | Account_opened of Account.t
    | Posted of Entry.t
  [@@deriving sexp_of, compare, equal]
end

type t =
  { seq : int
  ; event : Event.t
  }
[@@deriving sexp_of, compare, equal]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end

(** Hex [bin_shape] digest of {!Stable.V1.t}, written into new journal headers. *)
val digest : string

(** Digests whose journals this code reads, newest first; the head is {!digest}.

    Append-only, as in market-data's event log: a change to any type inside a record must
    keep old bytes decoding the same (add variant constructors at the end only), add its
    new digest at the head with a comment, and check in a journal written by the old code
    with a test that reads it. An incompatible change needs [Stable.V2]. *)
val compatible_digests : string list
