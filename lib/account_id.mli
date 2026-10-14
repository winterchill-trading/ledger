(** Account identifiers, e.g. [customer:42/eur] or [external:sepa].

    Every account that can hold funds has a pending sub-account, [<id>/pending], where
    {!Hold}s park reserved funds. The suffix is reserved: opening an account refuses ids
    ending in it, and ids longer than {!max_parent_length} so the sub-account id still
    fits {!Validated_id.max_length}. *)

open! Core
include Validated_id.S

(** [Validated_id.max_length - String.length "/pending"]. *)
val max_parent_length : int

(** [pending t] is the pending sub-account of [t]. *)
val pending : t -> t

(** Whether [t] is a pending sub-account ([<id>/pending]). *)
val is_pending : t -> bool

(** The account a pending sub-account belongs to. *)
val parent_of_pending : t -> t option
