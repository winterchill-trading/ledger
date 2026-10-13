(** An entry that undoes an earlier transfer: the same postings on the opposite sides,
    linked to the original by its sequence number ({!Entry.Kind.Reversal}).

    The original stays in the journal; the books show both, which is what an auditor
    expects. An entry can be reversed once. Hold entries are not reversed: release or
    capture the hold instead. Reversing a deposit that has already been spent fails like
    any other entry that would overdraw an account. *)

open! Core

val create
  :  Balances.t
  -> seq:int
  -> idempotency_key:Idempotency_key.t
  -> ?reference:string (** default: the original's reference *)
  -> effective_at:Time_ns.t
  -> recorded_at:Time_ns.t
  -> unit
  -> Entry.t Or_error.t
