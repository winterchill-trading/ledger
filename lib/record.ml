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

(* Literal, not [digest]: when a type inside a record changes, the computed digest moves
   and the pinned test in test_journal.ml fails until the change is classified (see the
   .mli). *)
let compatible_digests =
  [ (* V1, 2026-10-13: accounts, entries with holds and reversals. *)
    "3d66d278d2cb87148044dbc25f542224"
  ]
;;
