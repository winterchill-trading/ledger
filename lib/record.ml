open! Core

module Stable = struct
  module V1 = struct
    module Event = struct
      type t =
        | Account_opened of Account.Stable.V1.t
        | Posted of Entry.Stable.V1.t
        | Linked of Entry.Stable.V1.t list
        | Account_status_changed of
            { account : Account_id.Stable.V1.t
            ; status : Account_status.Stable.V1.t
            ; recorded_at : Time_ns.Stable.Alternate_sexp.V1.t
            }
        | Rejected of
            { idempotency_key : Idempotency_key.Stable.V1.t
            ; reason : string
            ; recorded_at : Time_ns.Stable.Alternate_sexp.V1.t
            }
        | Day_closed of
            { business_date : Date.Stable.V1.t
            ; recorded_at : Time_ns.Stable.Alternate_sexp.V1.t
            }
        | Adjustment_proposed of
            { entry : Entry.Stable.V1.t
            ; maker : string
            ; reason : string
            ; ticket : string
            }
        | Adjustment_approved of Entry.Stable.V1.t
        | Adjustment_rejected of
            { idempotency_key : Idempotency_key.Stable.V1.t
            ; checker : string
            ; recorded_at : Time_ns.Stable.Alternate_sexp.V1.t
            }
      [@@deriving bin_io, sexp, compare, equal]
    end

    type t =
      { seq : int
      ; event : Event.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

module Event = struct
  include Stable.V1.Event

  let recorded_at = function
    | Account_opened _ -> Time_ns.epoch
    | Posted e | Adjustment_approved e -> e.recorded_at
    | Linked es ->
      (match List.last es with
       | Some e -> e.recorded_at
       | None -> Time_ns.epoch)
    | Adjustment_proposed { entry; _ } -> entry.recorded_at
    | Account_status_changed { recorded_at; _ }
    | Adjustment_rejected { recorded_at; _ }
    | Rejected { recorded_at; _ }
    | Day_closed { recorded_at; _ } -> recorded_at
  ;;
end

type t = Stable.V1.t =
  { seq : int
  ; event : Event.t
  }
[@@deriving sexp_of, compare, equal]

let entries t =
  match t.event with
  | Posted e | Adjustment_approved e -> [ e ]
  | Linked es -> es
  | Account_opened _
  | Account_status_changed _
  | Rejected _
  | Day_closed _
  | Adjustment_proposed _
  | Adjustment_rejected _ -> []
;;

let digest = Bin_prot.Shape.eval_to_digest_string Stable.V1.bin_shape_t

(* Literal, not [digest]: when a type inside a record changes, the computed digest moves
   and the pinned test in test_journal.ml fails until the change is classified (see the
   .mli). *)
let compatible_digests =
  [ (* V1, 2026-10-13: accounts and status, entries (single and linked) with holds,
       increments, expiry and reversals, rejections, adjustments, day close. Set before
       any journal outside tests was written; the pre-release shape
       3d66d278d2cb87148044dbc25f542224 is deliberately not accepted. *)
    "132f0e078ccf2b8aa2dafa81b3ab9d9b"
  ]
;;
