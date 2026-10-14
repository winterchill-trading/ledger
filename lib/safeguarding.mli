(** Safeguarding: an e-money or payment institution must hold, per currency, at least what
    it owes its customers in segregated accounts. Requirement is the booked balance of
    [Customer_funds] accounts; internal resource is the booked balance of
    [Safeguarding_asset] accounts in the ledger; external balance is what the bank or
    provider statement says those accounts hold. *)

open! Core

module Status : sig
  type t =
    | Balanced
    | Shortfall
    | Excess
  [@@deriving sexp_of, compare, equal]
end

module Recon : sig
  type t =
    { currency : Money.Currency.t
    ; business_date : Date.t
    ; requirement : Money.Amount.t
    ; internal_resource : Money.Amount.t
    ; external_balance : Money.Amount.t
    ; resource_minus_requirement : Money.Amount.t
    ; external_minus_internal : Money.Amount.t
    (** non-zero: a reconciliation break between the ledger and the statement *)
    ; status : Status.t (** of [internal_resource] against [requirement] *)
    ; proposed_top_up : Money.Amount.t
    (** what to move into safeguarding to cover a shortfall; zero otherwise *)
    }
  [@@deriving sexp_of, compare, equal]
end

(** The safeguarding position of one currency on [business_date], against the
    [external_balance] a bank or provider statement reports for the safeguarding accounts.
    Fails if [external_balance] is in another currency. *)
val reconcile
  :  Balances.t
  -> currency:Money.Currency.t
  -> business_date:Date.t
  -> external_balance:Money.Amount.t
  -> Recon.t Or_error.t

module Exposure : sig
  type t =
    { currency : Money.Currency.t
    ; customer_liability : Money.Amount.t
    ; assets_held : Money.Amount.t
    ; house_position : Money.Amount.t (** [assets_held - customer_liability] *)
    }
  [@@deriving sexp_of, compare, equal]
end

(** Per currency with any customer funds or safeguarding assets, sorted. *)
val exposures : Balances.t -> Exposure.t list
