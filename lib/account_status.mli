(** Lifecycle of an account, changed by journal records.

    - [Open]: normal.
    - [Frozen]: money may come in; nothing may go out (no outgoing postings, no new holds,
      no captures) except returning held funds (release, expiry). Reversible.
    - [Closed]: nothing may be booked except returning held funds. Only an account with
      zero balance and nothing held in every currency can be closed, and closing is final. *)

open! Core

type t =
  | Open
  | Frozen
  | Closed
[@@deriving sexp, compare, equal, enumerate]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
