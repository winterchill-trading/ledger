open! Core

module Purpose = struct
  module Stable = struct
    module V1 = struct
      type t =
        | General
        | Customer_funds
        | Safeguarding_asset
        | Own_funds
        | Fee_income
        | Clearing
      [@@deriving bin_io, sexp, compare, equal, enumerate]
    end
  end

  include Stable.V1
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Account_id.Stable.V1.t
      ; kind : Account_kind.Stable.V1.t
      ; purpose : Purpose.Stable.V1.t
      ; allow_negative : bool
      ; tags : string String.Stable.V1.Map.t
      }
    [@@deriving bin_io, sexp, compare, equal, fields ~getters]
  end
end

include Stable.V1

let create ?(purpose = Purpose.General) ?(allow_negative = false) ?(tags = []) id kind =
  { id
  ; kind
  ; purpose
  ; allow_negative
  ; tags = String.Map.of_alist_reduce tags ~f:(fun _ v -> v)
  }
;;
