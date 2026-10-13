(** An account statement: the postings to one account in one currency, with a running
    balance on the account's normal side.

    Lines come in journal (sequence) order. The time range filters on [effective_at]; the
    running balance always counts every earlier record, so a backdated entry shows the
    balance as of when it was booked, as on a bank statement.

    Pagination is by sequence number: pass the previous page's [next_after_seq] as
    [after_seq]. [limit] counts entries, and a page never splits the postings of one
    entry, so no line is skipped or repeated across pages. *)

open! Core

module Line : sig
  type t =
    { seq : int
    ; effective_at : Time_ns.Alternate_sexp.t
    ; recorded_at : Time_ns.Alternate_sexp.t
    ; kind : Entry.Kind.t
    ; reference : string
    ; side : Side.t
    ; amount : Money.Amount.t
    ; balance : Money.Amount.t (** running balance after this line *)
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { account : Account_id.t
  ; currency : Money.Currency.t
  ; opening : Money.Amount.t (** balance before the first line *)
  ; lines : Line.t list
  ; closing : Money.Amount.t (** balance after the last line *)
  ; next_after_seq : int option (** [Some seq] when more lines follow *)
  }
[@@deriving sexp_of, compare, equal]

(** [from] is inclusive, [until] exclusive. Fails when [account] is not opened in
    [records] (a pending sub-account counts as opened with its parent). *)
val create
  :  ?from:Time_ns.t
  -> ?until:Time_ns.t
  -> ?after_seq:int
  -> ?limit:int
  -> Record.t list
  -> account:Account_id.t
  -> currency:Money.Currency.t
  -> t Or_error.t

(** A fixed-width table. Times are printed in UTC. *)
val to_string_hum : t -> string
