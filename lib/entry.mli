(** A journal entry: a balanced set of postings in one currency.

    Balanced means the debits and the credits add up to the same amount. Every entry has
    an [idempotency_key] (the caller-chosen id of the entry: the journal books each key
    once and remembers refusals), the time it takes effect ([effective_at], the value
    date, e.g. a card transaction's date), the time the ledger recorded it ([recorded_at],
    assigned by the journal's clock), the [business_date] it is booked on, a free-form
    [reference] (a provider transaction id, an end-to-end payment id), [remittance_info]
    for the counterparty, a {!Code.t} saying why money moved and a {!Kind.t} saying how
    the ledger treats it. *)

open! Core

(** Why money moved; reports group by it. A stable enum, next to the free tag of
    {!Kind.Transfer}. *)
module Code : sig
  type t =
    | Deposit
    | Withdrawal
    | Transfer_intraledger
    | Fx_leg
    | Fee
    | Card_auth
    | Card_settlement
    | Refund
    | Reversal
    | Return
    | Recall
    | Adjustment of
        { maker : string
        ; checker : string
        ; reason : string
        ; ticket : string
        }
    (** a manual correction, booked only through {!Journal.propose_adjustment} and
        approval; maker and checker are different operators *)
    | Interest
    | Safeguarding_sweep
    | Hold (** a hold placed, captured, released or expired with no more specific code *)
  [@@deriving sexp, compare, equal]

  (** ["DEPOSIT"], ["CARD_SETTLEMENT"], ...: the proprietary bank transaction code printed
      on statements. *)
  val to_string : t -> string

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

module Kind : sig
  type t =
    | Transfer of string
    (** an ordinary entry; the string is a free tag such as [deposit] or [fee] *)
    | Hold_place of Hold_id.t (** see {!Hold.place} *)
    | Hold_release of Hold_id.t (** see {!Hold.release} *)
    | Hold_capture of Hold_id.t (** see {!Hold.capture} *)
    | Reversal of { reverses : Idempotency_key.t }
    (** undoes the entry booked under this key *)
    | Hold_place_expiring of
        { hold : Hold_id.t
        ; expires_at : Time_ns.Alternate_sexp.t
        } (** a hold that can no longer be captured or released after [expires_at] *)
    | Hold_expire of Hold_id.t
    (** returns an expired hold's remainder; see {!Hold.expire} *)
    | Hold_over_capture of
        { hold : Hold_id.t
        ; reason : string
        } (** a capture above the hold; the excess is debited from the account directly *)
    | Hold_increment of Hold_id.t
    (** adds to an open hold (an incremental card authorisation); see {!Hold.increment} *)
  [@@deriving sexp, compare, equal]

  (** The hold an entry of this kind acts on. *)
  val hold : t -> Hold_id.t option

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

type t = private
  { idempotency_key : Idempotency_key.t
  ; kind : Kind.t
  ; code : Code.t
  ; reference : string
  ; remittance_info : string
  ; effective_at : Time_ns.Alternate_sexp.t
  ; recorded_at : Time_ns.Alternate_sexp.t
  ; business_date : Date.t
  ; sca : Sca_evidence.t option
  ; postings : Posting.t list
  }
[@@deriving sexp_of, compare, equal]

(** Fails unless there are at least two postings, every amount is positive, all amounts
    are in one currency, debits equal credits and an [Adjustment]'s maker and checker are
    different non-empty names. *)
val create
  :  idempotency_key:Idempotency_key.t
  -> kind:Kind.t
  -> ?code:Code.t
       (** default: [Reversal] for reversals, [Hold] for hold entries,
           [Transfer_intraledger] otherwise *)
  -> ?reference:string (** default [""] *)
  -> ?remittance_info:string (** default [""] *)
  -> ?business_date:Date.t (** default: the UTC date of [recorded_at] *)
  -> ?sca:Sca_evidence.t
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> Posting.t list
  -> t Or_error.t

(** The journal assigns timestamps: it calls this with its own clock before booking. *)
val restamp : t -> recorded_at:Time_ns.t -> business_date:Date.t -> t

(** The same entry with another code, validated again. Used to book an approved
    adjustment. *)
val with_code : t -> Code.t -> t Or_error.t

val currency : t -> Money.Currency.t

(** Sum of the debit postings, which equals the sum of the credits. *)
val total : t -> Money.Amount.t

(** [same_request a b] is true when [a] and [b] differ at most in [recorded_at] and
    [business_date]: a retried request, not a conflicting use of the same key. *)
val same_request : t -> t -> bool

module Stable : sig
  module V1 : sig
    (** Decoding validates again, so a damaged or hand-made journal cannot smuggle in an
        unbalanced entry. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
