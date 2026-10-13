(** The state a journal folds to, and the ledger's rules.

    {!apply} is the only way to change it and enforces every rule, so a value built from
    any sequence of successful [apply]s satisfies:

    - each currency's debits equal its credits ({!Trial_balance} checks this);
    - no account without [allow_negative] is below zero on its normal side, counting what
      is held: [available] never goes negative for a customer account (the TigerBeetle
      flags [debits_must_not_exceed_credits] / [credits_must_not_exceed_debits], checked
      against posted plus pending);
    - the balance of every pending sub-account equals the sum of its open holds;
    - each idempotency key is booked or refused once, and a refused key stays refused;
    - each entry is reversed at most once; each hold is captured, released or expired
      until nothing remains, and never after it expired;
    - [recorded_at] and [business_date] never go backwards, and nothing is booked on a
      closed business date;
    - frozen and closed accounts only receive money or get held funds back.

    The value is immutable, so a caller can try an entry and drop the result. *)

open! Core

type t

val empty : t

(** The sequence number the next record must carry (1 for an empty journal). *)
val next_seq : t -> int

(** Validates [record] against the rules above and returns the new state, or an error
    saying which rule it breaks. *)
val apply : t -> Record.t -> t Or_error.t

(** [List.fold_result ~init:empty ~f:apply]. *)
val of_records : Record.t list -> t Or_error.t

(** Timestamp of the last record applied. *)
val last_recorded_at : t -> Time_ns.t option

(** {2 Accounts} *)

(** Open accounts, sorted by id, without the implicit pending sub-accounts. *)
val accounts : t -> Account.t list

(** The account [id] names. A pending sub-account of an open account resolves to its
    parent's record with the pending id, its kind, and [allow_negative = false]. *)
val find_account : t -> Account_id.t -> Account.t option

val status : t -> Account_id.t -> Account_status.t

(** {2 Balances}

    Balances are on the account's normal side: a deposit of 10 EUR into a customer
    (Liability) account reads as 10 EUR, as does 10 EUR of cash in an Asset account. *)

(** Balance of exactly this account (no sub-accounts). *)
val balance : t -> Account_id.t -> Money.Currency.t -> Money.Amount.t

(** Funds the holder may spend: [balance] of the account itself, since held funds have
    moved to the pending sub-account. *)
val available : t -> Account_id.t -> Money.Currency.t -> Money.Amount.t

(** Funds reserved by open holds: the balance of the pending sub-account. *)
val held : t -> Account_id.t -> Money.Currency.t -> Money.Amount.t

(** [available + held]: the booked balance. *)
val posted : t -> Account_id.t -> Money.Currency.t -> Money.Amount.t

(** Cumulative totals of one account in one currency, in the shape TigerBeetle keeps them:
    posted and pending debits and credits, each never decreasing except pending columns
    when a hold resolves. Movements between an account and its own pending sub-account are
    not counted as posted: placing a hold adds to a pending column, releasing it takes it
    away again, capturing moves it to a posted column. *)
module Columns : sig
  type t =
    { debits_posted : Money.Amount.t
    ; credits_posted : Money.Amount.t
    ; debits_pending : Money.Amount.t
    ; credits_pending : Money.Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

val columns : t -> Account_id.t -> Money.Currency.t -> Columns.t

(** Debits minus credits for every account (pending sub-accounts separately) and currency
    with postings, sorted. *)
val debits_minus_credits : t -> (Account_id.t * Money.Currency.t * Money.Amount.t) list

(** Currencies in which [account] or its pending sub-account has postings. *)
val currencies : t -> Account_id.t -> Money.Currency.t list

(** {2 Holds} *)

module Hold_state : sig
  type t =
    { id : Hold_id.t
    ; account : Account_id.t (** the account the funds are reserved from *)
    ; placed : Money.Amount.t
    ; remaining : Money.Amount.t (** zero once fully captured, released or expired *)
    ; placed_by : Idempotency_key.t
    ; expires_at : Time_ns.Alternate_sexp.t option
    }
  [@@deriving sexp_of, compare, equal]
end

(** Holds, open or closed; hold ids are never reused. *)
val find_hold : t -> Hold_id.t -> Hold_state.t option

(** Holds with a positive [remaining], sorted by id. *)
val open_holds : t -> Hold_state.t list

(** Open holds whose [expires_at] is at or before [now]. *)
val expired_holds : t -> now:Time_ns.t -> Hold_state.t list

(** {2 Entries and keys} *)

(** The entry booked under this key and the sequence number of its record. *)
val find_entry : t -> Idempotency_key.t -> (int * Entry.t) option

(** Sequence number of the record that booked this key. *)
val find_idempotency_key : t -> Idempotency_key.t -> int option

(** Why an entry under this key was refused, and the sequence number of the refusal. *)
val find_rejection : t -> Idempotency_key.t -> (int * string) option

(** Key of the reversal of the entry booked under this key. *)
val reversed_by : t -> Idempotency_key.t -> Idempotency_key.t option

(** {2 Adjustments}

    Manual corrections follow four eyes: an operator proposes an entry, a different
    operator approves it within {!proposal_lifetime} and only then is it booked, with code
    [Adjustment { maker; checker; reason; ticket }]. An entry with that code is refused
    anywhere else. *)

module Proposal : sig
  module Status : sig
    type t =
      | Proposed
      | Approved
      | Rejected
    [@@deriving sexp_of, compare, equal]
  end

  type t =
    { entry : Entry.t
    ; maker : string
    ; reason : string
    ; ticket : string
    ; proposed_at : Time_ns.Alternate_sexp.t
    ; status : Status.t
    }
  [@@deriving sexp_of, compare, equal]

  (** A proposal not decided within {!proposal_lifetime} can no longer be approved. *)
  val is_expired : t -> now:Time_ns.t -> bool
end

(** 24 hours. *)
val proposal_lifetime : Time_ns.Span.t

val find_proposal : t -> Idempotency_key.t -> Proposal.t option

(** Proposals still waiting for a decision (expired ones included), sorted by key. *)
val open_proposals : t -> Proposal.t list

(** {2 Business days} *)

(** The last business date closed, if any. *)
val closed_through : t -> Date.t option

(** Per account and currency, the day's movements since the previous close: what a close
    snapshot records and what camt.053 calls the opening (OPBD) and closing booked (CLBD)
    and closing available (CLAV) balances. *)
module Day_row : sig
  type t =
    { account : Account_id.t
    ; currency : Money.Currency.t
    ; opening_booked : Money.Amount.t
    ; debits : Money.Amount.t (** posted debits since the previous close *)
    ; credits : Money.Amount.t
    ; closing_booked : Money.Amount.t
    ; closing_available : Money.Amount.t
    }
  [@@deriving sexp_of, compare, equal]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

(** Rows for every account and currency with postings, sorted. *)
val day_rows : t -> Day_row.t list

(** {2 Serialization}

    The complete state, so a snapshot can restart replay where it was taken. Decoding
    re-checks nothing; snapshots carry their own checks (see {!Snapshot}). *)
module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp_of]
  end
end

(** Equal states, compared through their serialized form. *)
val equal : t -> t -> bool
