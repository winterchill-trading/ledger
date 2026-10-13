(** The state a journal folds to: open accounts, balances per account per currency, holds
    and the bookkeeping needed to validate the next record.

    {!apply} is the only way to change it and enforces every ledger rule, so a value of
    this type built from any sequence of successful [apply]s satisfies:

    - each currency's debits equal its credits ({!Trial_balance} checks this);
    - no account without [allow_negative] is below zero on its normal side, so [available]
      never goes negative for a customer account;
    - the balance of every pending sub-account equals the sum of its open holds;
    - each idempotency key is booked once, each entry reversed at most once.

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

(** {2 Accounts} *)

(** Open accounts, sorted by id, without the implicit pending sub-accounts. *)
val accounts : t -> Account.t list

(** The account [id] names. A pending sub-account of an open account resolves to its
    parent's record with the pending id, its kind, and [allow_negative = false]. *)
val find_account : t -> Account_id.t -> Account.t option

(** {2 Balances}

    All balances are on the account's normal side: a deposit of 10 EUR into a customer
    (Liability) account reads as 10 EUR, as does 10 EUR of cash in an Asset account. *)

(** Balance of exactly this account (no sub-accounts). Zero for an account without
    postings in [currency]. *)
val balance : t -> Account_id.t -> Money.Currency.t -> Money.Amount.t

(** Funds the account holder may spend: [balance] of the account itself, since held funds
    have moved to the pending sub-account. *)
val available : t -> Account_id.t -> Money.Currency.t -> Money.Amount.t

(** Funds reserved by open holds: the balance of the pending sub-account. *)
val held : t -> Account_id.t -> Money.Currency.t -> Money.Amount.t

(** [available + held]. *)
val posted : t -> Account_id.t -> Money.Currency.t -> Money.Amount.t

(** Debits minus credits for every account and currency with postings, sorted. Pending
    sub-accounts are listed separately. *)
val debits_minus_credits : t -> (Account_id.t * Money.Currency.t * Money.Amount.t) list

(** {2 Holds} *)

module Hold_state : sig
  type t =
    { id : Hold_id.t
    ; account : Account_id.t (** the account the funds are reserved from *)
    ; placed : Money.Amount.t
    ; remaining : Money.Amount.t (** zero once fully captured or released *)
    ; placed_seq : int
    }
  [@@deriving sexp_of, compare, equal]
end

(** Holds, open or closed; hold ids are never reused. *)
val find_hold : t -> Hold_id.t -> Hold_state.t option

(** Holds with a positive [remaining], sorted by id. *)
val open_holds : t -> Hold_state.t list

(** {2 Entries} *)

val find_entry : t -> seq:int -> Entry.t option

(** Sequence number of the entry booked under this key. *)
val find_idempotency_key : t -> Idempotency_key.t -> int option

(** Sequence number of the reversal of the entry at [seq]. *)
val reversed_by : t -> seq:int -> int option
