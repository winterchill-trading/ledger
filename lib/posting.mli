(** One line of a journal entry: an amount on one side of one account. *)

open! Core

type t =
  { account : Account_id.t
  ; side : Side.t
  ; amount : Money.Amount.t (** always positive; the side carries the direction *)
  }
[@@deriving sexp, compare, equal]

(** [debit account amount]; [amount] must be positive, which {!Entry.create} checks. *)
val debit : Account_id.t -> Money.Amount.t -> t

(** [credit account amount], as {!debit}. *)
val credit : Account_id.t -> Money.Amount.t -> t

(** The same amount on the other side. *)
val flip : t -> t

(** [amount] for a debit, minus [amount] for a credit. Balances are kept in these terms
    (debits minus credits) and turned into an account's normal-side view on the way out. *)
val signed : t -> Money.Amount.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
