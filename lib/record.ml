open! Core

module Stable = struct
  module V1 = struct
    module Event = struct
      type t =
        | Account_opened of Account.Stable.V1.t
        | Posted of Entry.Stable.V1.t
      [@@deriving bin_io, sexp, compare, equal]
    end

    type t =
      { seq : int
      ; event : Event.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

module Event = Stable.V1.Event

type t = Stable.V1.t =
  { seq : int
  ; event : Event.t
  }
[@@deriving sexp_of, compare, equal]

let digest = Bin_prot.Shape.eval_to_digest_string Stable.V1.bin_shape_t
let compatible_digests = [ digest ]
