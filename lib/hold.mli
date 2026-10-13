(** Holds: funds reserved for a payment that has not settled yet (a card authorisation, an
    outgoing transfer waiting for the payment network).

    A hold is three kinds of ordinary, balanced entries on the account's pending
    sub-account ({!Account_id.pending}):

    - [place] moves the amount from the account to its pending sub-account, so
      {!Balances.available} drops at once while {!Balances.posted} does not;
    - [capture] moves some or all of it from the pending sub-account to the payee (the
      payment settled);
    - [release] moves some or all of it back (the authorisation expired or was reduced).

    These functions only build the entry from the current state; {!Balances.apply} (via
    {!Journal}) checks it again when it is booked. Placing a hold larger than the
    available balance fails there unless the account allows negative balances. *)

open! Core

val place
  :  Balances.t
  -> hold_id:Hold_id.t
  -> account:Account_id.t
  -> amount:Money.Amount.t
  -> idempotency_key:Idempotency_key.t
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
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t

(** Pays [amount] (default: all that remains) to [to_]. A capture of less than the hold
    leaves the rest held until it is released or captured. *)
val capture
  :  Balances.t
  -> hold_id:Hold_id.t
  -> ?amount:Money.Amount.t
  -> to_:Account_id.t
  -> idempotency_key:Idempotency_key.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t
