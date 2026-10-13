(** The key that makes posting an entry safe to retry: the journal books each key at most
    once. Provider events use their provider event id, so a webhook delivered twice is
    booked once. *)

open! Core
include Validated_id.S
