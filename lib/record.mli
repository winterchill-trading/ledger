(** One record of the journal, numbered from 1 with no gaps. *)

open! Core

module Event : sig
  type t =
    | Account_opened of Account.t
    | Posted of Entry.t
    | Linked of Entry.t list
    (** entries booked together or not at all (an FX swap's two legs and its fee) *)
    | Account_status_changed of
        { account : Account_id.t
        ; status : Account_status.t
        ; recorded_at : Time_ns.Alternate_sexp.t
        }
    | Rejected of
        { idempotency_key : Idempotency_key.t
        ; reason : string
        ; recorded_at : Time_ns.Alternate_sexp.t
        }
    (** an entry refused for a reason that depends on the ledger's state at that moment
        (insufficient funds, a frozen account, an expired hold); retries under the same
        key get the same refusal *)
    | Day_closed of
        { business_date : Date.t
        ; recorded_at : Time_ns.Alternate_sexp.t
        } (** end of day: nothing more may be booked on or before [business_date] *)
    | Adjustment_proposed of
        { entry : Entry.t
        ; maker : string
        ; reason : string
        ; ticket : string
        }
    (** a manual correction waiting for a second operator; nothing is booked yet (see
        {!Journal.propose_adjustment}) *)
    | Adjustment_approved of Entry.t
    (** the proposed entry, restamped and with code [Adjustment { maker; checker; ... }],
        now booked *)
    | Adjustment_rejected of
        { idempotency_key : Idempotency_key.t (** the proposed entry's key *)
        ; checker : string
        ; recorded_at : Time_ns.Alternate_sexp.t
        }
  [@@deriving sexp_of, compare, equal]

  (** The ledger timestamp of the event (of the last entry for [Linked]). *)
  val recorded_at : t -> Time_ns.t
end

type t =
  { seq : int
  ; event : Event.t
  }
[@@deriving sexp_of, compare, equal]

(** The entries a record books, in order: one for [Posted] and [Adjustment_approved], all
    of a [Linked] group, none otherwise. *)
val entries : t -> Entry.t list

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end

(** Hex [bin_shape] digest of {!Stable.V1.t}, written into new journal headers. *)
val digest : string

(** Digests whose journals this code reads, newest first; the head is {!digest}.

    Append-only, as in market-data's event log: a change to any type inside a record must
    keep old bytes decoding the same (add variant constructors at the end only), add its
    new digest at the head with a comment, and check in a journal written by the old code
    with a test that reads it. An incompatible change needs [Stable.V2]. *)
val compatible_digests : string list
