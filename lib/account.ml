open! Core

module Stable = struct
  module V1 = struct
    type t =
      { id : Account_id.Stable.V1.t
      ; kind : Account_kind.Stable.V1.t
      ; allow_negative : bool
      ; tags : string String.Stable.V1.Map.t
      }
    [@@deriving bin_io, sexp, compare, equal, fields ~getters]
  end
end

include Stable.V1

let create ?(allow_negative = false) ?(tags = []) id kind =
  { id; kind; allow_negative; tags = String.Map.of_alist_reduce tags ~f:(fun _ v -> v) }
;;
