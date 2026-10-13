(** A journal entry: a balanced set of postings in one currency.

    Balanced means the debits and the credits add up to the same amount. Every entry has
    an [idempotency_key] (the journal books each key once), the time it takes effect
    ([effective_at], e.g. a card transaction's date) and the time the ledger recorded it
    ([recorded_at], from the ledger's clock), a free-form [reference] (a provider
    transaction id, an end-to-end payment id) and a {!Kind.t}. *)

open! Core

module Kind : sig
  type t =
    | Transfer of string
    (** an ordinary entry; the string is a free tag such as [deposit] or [fee] *)
    | Hold_place of Hold_id.t (** see {!Hold.place} *)
    | Hold_release of Hold_id.t (** see {!Hold.release} *)
    | Hold_capture of Hold_id.t (** see {!Hold.capture} *)
    | Reversal of { reverses : int } (** undoes the entry with this sequence number *)
  [@@deriving sexp, compare, equal]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

type t = private
  { idempotency_key : Idempotency_key.t
  ; kind : Kind.t
  ; reference : string
  ; effective_at : Time_ns.Alternate_sexp.t
  ; recorded_at : Time_ns.Alternate_sexp.t
  ; postings : Posting.t list
  }
[@@deriving sexp_of, compare, equal]

(** Fails unless there are at least two postings, every amount is positive, all amounts
    are in one currency and debits equal credits. *)
val create
  :  idempotency_key:Idempotency_key.t
  -> kind:Kind.t
  -> ?reference:string (** default [""] *)
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> Posting.t list
  -> t Or_error.t

val currency : t -> Money.Currency.t

(** Sum of the debit postings, which equals the sum of the credits. *)
val total : t -> Money.Amount.t

(** [same_request a b] is true when [a] and [b] differ at most in [recorded_at]: a retried
    request, not a conflicting use of the same idempotency key. *)
val same_request : t -> t -> bool

module Stable : sig
  module V1 : sig
    (** Decoding validates again, so a damaged or hand-made journal cannot smuggle in an
        unbalanced entry. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
