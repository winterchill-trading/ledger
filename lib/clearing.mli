(** Clearing accounts (purpose [Clearing]) carry money through one flow, e.g. SEPA out:
    debited when the customer pays, credited when the network confirms. When a flow
    completes they return to zero, so a balance that stays non-zero is the sign of a
    missing, late or wrong event. This report lists them with their age. *)

open! Core

module Item : sig
  type t =
    { account : Account_id.t
    ; currency : Money.Currency.t
    ; balance : Money.Amount.t (** on the account's normal side *)
    ; non_zero_since : Date.t (** business date of the entry that left it non-zero *)
    ; age_days : int (** [as_of - non_zero_since] *)
    }
  [@@deriving sexp_of, compare, equal]
end

(** Clearing accounts with a non-zero balance after the journal, oldest first. With
    [older_than_days], only items at least that old (1 lists what did not clear within a
    business day). *)
val open_items
  :  ?older_than_days:int
  -> Record.t list
  -> as_of:Date.t
  -> Item.t list Or_error.t
