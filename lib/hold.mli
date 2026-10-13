(** Holds: funds reserved for a payment that has not settled yet (a card authorisation, an
    outgoing transfer waiting for the payment network). This is the two-phase transfer of
    TigerBeetle (pending, then post or void, with an optional timeout), expressed as
    ordinary balanced entries on the account's pending sub-account
    ({!Account_id.pending}):

    - [place] moves the amount from the account to its pending sub-account, so
      {!Balances.available} drops at once while {!Balances.posted} does not;
    - [capture] moves some or all of it to payees (the payment settled). By default the
      rest stays held for later captures (a card payment cleared in several parts);
      [~release_remainder:true] returns it in the same entry, as a TigerBeetle partial
      post does;
    - [over_capture] takes the whole hold plus an excess straight from the account, and
      needs a reason (a tip, a currency difference at settlement);
    - [release] returns some or all of it (the authorisation was cancelled or reduced);
    - [expire] returns the rest of a hold placed with [expires_at] once that time has
      passed; an expired hold can no longer be captured or released.

    These functions only build the entry from the current state; {!Balances.apply} (via
    {!Journal}) checks it again when it is booked. Placing a hold larger than the
    available balance fails there unless the account allows negative balances. *)

open! Core

val place
  :  Balances.t
  -> hold_id:Hold_id.t
  -> account:Account_id.t
  -> amount:Money.Amount.t
  -> ?expires_at:Time_ns.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> ?remittance_info:string
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t

(** Adds [amount] to an open hold (an incremental authorisation, e.g. a hotel stay
    extended). *)
val increment
  :  Balances.t
  -> hold_id:Hold_id.t
  -> amount:Money.Amount.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t

(** Releases [amount] (default: all that remains) back to the account. *)
val release
  :  Balances.t
  -> hold_id:Hold_id.t
  -> ?amount:Money.Amount.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t

(** Pays [amount] (default: all that remains) to [to_]. *)
val capture
  :  Balances.t
  -> hold_id:Hold_id.t
  -> ?amount:Money.Amount.t
  -> ?release_remainder:bool (** default [false] *)
  -> to_:Account_id.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> ?remittance_info:string
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t

(** Pays several payees from one hold in one entry (the merchant and a fee account). *)
val capture_split
  :  Balances.t
  -> hold_id:Hold_id.t
  -> ?release_remainder:bool (** default [false] *)
  -> (Account_id.t * Money.Amount.t) list
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> ?remittance_info:string
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t

(** Pays [amount], more than the hold's remaining amount, to [to_]: the hold is used up
    and the excess is debited from the account (subject to its balance rules). *)
val over_capture
  :  Balances.t
  -> hold_id:Hold_id.t
  -> amount:Money.Amount.t
  -> reason:string
  -> to_:Account_id.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> ?remittance_info:string
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t

(** Returns what remains of an expired hold. *)
val expire
  :  Balances.t
  -> hold_id:Hold_id.t
  -> idempotency_key:Idempotency_key.t
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t
