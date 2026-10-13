(** An account in the chart of accounts.

    Accounts are not tied to a currency: balances are kept per account per currency, and a
    customer's EUR and BTC wallets can be one account or two, as the caller prefers.

    [allow_negative] is for system and external accounts only (a settlement account at a
    payment network, the bank's own funding account): the ledger refuses any entry or hold
    that would take an account without it below zero on its normal side. *)

open! Core

(** What the account is for, so safeguarding and clearing reports can find them. Ids, not
    people: the ledger stores no personal data, so customer accounts are named by an
    opaque customer id and tags hold ids too. *)
module Purpose : sig
  type t =
    | General
    | Customer_funds (** what the institution owes a customer; a Liability *)
    | Safeguarding_asset
    (** money held at a bank or provider to cover customer funds; an Asset *)
    | Own_funds
    | Fee_income
    | Clearing
    (** a transit account for one flow (SEPA out, cards, FX) that must return to zero *)
  [@@deriving sexp, compare, equal, enumerate]
end

type t =
  { id : Account_id.t
  ; kind : Account_kind.t
  ; purpose : Purpose.t
  ; allow_negative : bool
  ; tags : string String.Map.t (** free labels, e.g. [customer], [product] *)
  }
[@@deriving sexp, compare, equal, fields ~getters]

val create
  :  ?purpose:Purpose.t (** default [General] *)
  -> ?allow_negative:bool (** default [false] *)
  -> ?tags:(string * string) list
  -> Account_id.t
  -> Account_kind.t
  -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
