(** How a payment was authorised under strong customer authentication (PSD2): the id of
    the challenge the customer passed, or the exemption that was applied. Kept on the
    entry so liability for an unauthorised payment (PSD2 Art. 74(2)) can be traced. *)

open! Core

type t =
  | Challenge of string (** the SCA challenge id *)
  | Exemption of string (** the exemption, e.g. [low_value], [trusted_beneficiary] *)
[@@deriving sexp, compare, equal]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
