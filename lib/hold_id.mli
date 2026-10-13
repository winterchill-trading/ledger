(** Identifies one {!Hold} (an authorisation, a pending card payment, a payment awaiting
    settlement). Chosen by the caller, usually the provider's authorisation id. *)

open! Core
include Validated_id.S
