(** The five account classes of double-entry bookkeeping.

    Asset and Expense accounts are debit-normal: a debit increases them. Liability, Equity
    and Income accounts are credit-normal. A bank's customer accounts are Liabilities (the
    bank owes the customer the money), so a deposit credits them. *)

open! Core

type t =
  | Asset
  | Liability
  | Equity
  | Income
  | Expense
[@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

(** The side that increases an account of this kind. *)
val normal_side : t -> Side.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
