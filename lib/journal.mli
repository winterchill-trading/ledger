(** The ledger's system of record: an append-only file of {!Record.t}s ({!Framing}
    layout), with the folded {!Balances.t} kept in memory.

    Every append is validated by {!Balances.apply} before it is written, written as one
    frame, and (by default) [fsync]ed before the call returns, so a record the caller saw
    succeed survives a crash. One process at a time may hold a journal open for writing
    (an exclusive [lockf] lock); readers ({!read}, {!replay}) take no lock.

    Reopening replays the whole file: a damaged frame, a record that does not follow its
    predecessor's sequence number, or one that breaks a ledger rule is an error. A
    truncated last frame (a crash during an append, which therefore never returned
    success) is an error too unless [repair_truncated_tail] is set, in which case it is
    cut off.

    If a write fails half way the journal refuses further appends; close it and reopen
    (with [repair_truncated_tail]) to continue.

    The whole journal is read into memory on open; it is meant for ledgers whose history
    fits in memory, which the in-memory balances need anyway. *)

open! Core
module Time_source = Async_kernel.Time_source

type t

(** What an append did. A request repeated with the same idempotency key is not booked
    again: it returns [Duplicate] with the sequence number of the first booking, so
    callers (webhook handlers, retries after a timeout) can treat it as success. Reusing a
    key for a {e different} entry is an error. *)
module Append_result : sig
  type t =
    | Appended of Record.t
    | Duplicate of { seq : int }
  [@@deriving sexp_of, compare, equal]

  val seq : t -> int
end

(** Creates a new journal file; fails if [path] exists. [time_source] (default the wall
    clock) stamps [recorded_at]; [fsync] (default [true]) syncs each append. *)
val create
  :  ?time_source:[> read ] Time_source.T1.t
  -> ?fsync:bool
  -> string
  -> t Or_error.t

(** Opens an existing journal for appending, replaying it. *)
val open_
  :  ?time_source:[> read ] Time_source.T1.t
  -> ?fsync:bool
  -> ?repair_truncated_tail:bool (** default [false] *)
  -> string
  -> t Or_error.t

(** Releases the lock and the file. Idempotent; later appends fail. *)
val close : t -> unit

val path : t -> string
val balances : t -> Balances.t

(** All records in sequence order. *)
val records : t -> Record.t list

(** The journal's clock. *)
val now : t -> Time_ns.t

(** {2 Appending} *)

(** Opening the same account again with an identical record is a [Duplicate]. *)
val open_account : t -> Account.t -> Append_result.t Or_error.t

(** Books an entry built elsewhere (its [recorded_at] is kept as given). *)
val append_entry : t -> Entry.t -> Append_result.t Or_error.t

(** Books an ordinary entry; [tag] (default ["transfer"]) becomes
    [Entry.Kind.Transfer tag]. *)
val transfer
  :  t
  -> idempotency_key:Idempotency_key.t
  -> ?tag:string
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> Posting.t list
  -> Append_result.t Or_error.t

(** {!Hold.place} stamped with {!now} and booked. *)
val hold
  :  t
  -> hold_id:Hold_id.t
  -> account:Account_id.t
  -> amount:Money.Amount.t
  -> idempotency_key:Idempotency_key.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

val release
  :  t
  -> hold_id:Hold_id.t
  -> ?amount:Money.Amount.t
  -> idempotency_key:Idempotency_key.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

val capture
  :  t
  -> hold_id:Hold_id.t
  -> ?amount:Money.Amount.t
  -> to_:Account_id.t
  -> idempotency_key:Idempotency_key.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

(** {!Reversal.create} stamped with {!now} and booked. *)
val reverse
  :  t
  -> seq:int
  -> idempotency_key:Idempotency_key.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

(** {2 Reading without a lock} *)

(** The records of a journal file and what follows the last intact frame. Fails on a bad
    header or a record that breaks a ledger rule. *)
val read : string -> (Record.t list * Framing.Tail.t) Or_error.t

(** Folds a journal file to its balances. Fails like {!read}, and on a corrupt (but not a
    truncated) tail. *)
val replay : string -> Balances.t Or_error.t
