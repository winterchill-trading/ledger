(** Which side of an account a posting lands on. *)

open! Core

type t =
  | Debit
  | Credit
[@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

val flip : t -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
