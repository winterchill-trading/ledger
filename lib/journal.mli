(** The ledger's system of record: an append-only file of {!Record.t}s ({!Framing}
    layout), with the folded {!Balances.t} kept in memory.

    Every append is validated by {!Balances.apply} before it is written, written as one
    frame, and (by default) [fsync]ed before the call returns, so a record the caller saw
    succeed survives a crash. One process at a time may hold a journal open for writing
    (an exclusive [lockf] lock); readers ({!read}, {!replay}) take no lock.

    {2 Timestamps and business dates}

    The ledger assigns time, as TigerBeetle does: every entry is restamped with the
    journal's clock ([recorded_at], strictly increasing even if the clock stalls or steps
    back) and booked on the current business date: the UTC date of [recorded_at], or the
    day after the last close if that is later. The caller's [effective_at] (value date) is
    kept as given.

    {2 Idempotency}

    The idempotency key is the caller's id for the entry. A retry with the same key and
    the same content returns [Duplicate] with the first booking's sequence number; reusing
    a key for different content is an error. An entry that {!Balances.apply} refuses
    (insufficient funds, frozen account, expired hold, ...) is remembered with a
    [Rejected] record, so a retry under the same key gets the same refusal even if the
    state has changed since; a caller that wants a new attempt uses a new key. Errors that
    happen before an entry exists (a malformed entry, an unknown hold id passed to a
    builder) are not recorded.

    {2 Opening}

    Reopening replays the whole file, or only the part after a {!Snapshot}: a damaged
    frame, a record that does not follow its predecessor's sequence number, or one that
    breaks a ledger rule is an error. A truncated last frame (a crash during an append,
    which therefore never returned success) is an error too unless [repair_truncated_tail]
    is set, in which case it is cut off. If a write fails half way the journal refuses
    further appends; close it and reopen.

    {2 Memory}

    Opening streams the file one frame at a time ({!Framing.fold}); what stays in memory
    is the folded {!Balances.t} (which holds every booked entry, for idempotency) and, by
    default, the list of records for {!records}. With [~keep_records:false] the list is
    not kept and {!records} reads the file again when called. {!fold}, {!replay} and the
    statement-style readers never hold the whole file. *)

open! Core
module Time_source = Async_kernel.Time_source

type t

module Append_result : sig
  type t =
    | Appended of Record.t
    | Duplicate of { seq : int }
  [@@deriving sexp_of, compare, equal]

  val seq : t -> int
end

(** Creates a new journal file; fails if [path] exists. [time_source] (default the wall
    clock) stamps [recorded_at]; [fsync] (default [true]) syncs each append;
    [remember_refusals] (default [true]) writes a [Rejected] record for each refused
    entry; [keep_records] (default [true]) keeps the records in memory for {!records}. *)
val create
  :  ?time_source:[> read ] Time_source.T1.t
  -> ?fsync:bool
  -> ?remember_refusals:bool
  -> ?keep_records:bool
  -> string
  -> t Or_error.t

(** Opens an existing journal for appending. With [snapshot], the state is taken from the
    snapshot (after checking that the journal starts with the bytes it covers) and only
    the records after it are replayed. The file is read frame by frame; see {e Memory}
    above for [keep_records]. *)
val open_
  :  ?time_source:[> read ] Time_source.T1.t
  -> ?fsync:bool
  -> ?remember_refusals:bool
  -> ?keep_records:bool
  -> ?repair_truncated_tail:bool (** default [false] *)
  -> ?snapshot:Snapshot.t
  -> string
  -> t Or_error.t

(** Releases the lock and the file. Idempotent; later appends fail. *)
val close : t -> unit

val path : t -> string
val balances : t -> Balances.t

(** Records in sequence order: all of them, or those after the snapshot the journal was
    opened from. With [~keep_records:false] this reads them from the file on each call,
    and raises if the file cannot be read back (it was damaged after opening). *)
val records : t -> Record.t list

(** The hash-chain head after the last record written: publish
    [Chain_head.to_string (chain_head t)] somewhere outside the system (after each close,
    say) so that cutting records off the end of the file can be detected with
    {!Chain_head.verify}. *)
val chain_head : t -> Chain_head.t

(** The time the next record would be stamped with: the clock, or just after the last
    record if the clock is behind. *)
val now : t -> Time_ns.t

(** The business date the next entry would be booked on. *)
val business_date : t -> Date.t

(** {2 Accounts} *)

(** Opening the same account again with an identical record is a [Duplicate]. *)
val open_account : t -> Account.t -> Append_result.t Or_error.t

val set_status : t -> Account_id.t -> Account_status.t -> Append_result.t Or_error.t

(** {2 Entries} *)

(** Books an entry built elsewhere, restamped with the journal's time and business date. *)
val append_entry : t -> Entry.t -> Append_result.t Or_error.t

(** Books entries together in one record: all or none. The group is a [Duplicate] when
    every key was booked by one earlier group with the same content. *)
val append_linked : t -> Entry.t list -> Append_result.t Or_error.t

(** Books an ordinary entry; [tag] (default ["transfer"]) becomes
    [Entry.Kind.Transfer tag]. *)
val transfer
  :  t
  -> idempotency_key:Idempotency_key.t
  -> ?tag:string
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> ?remittance_info:string
  -> effective_at:Time_ns.t
  -> Posting.t list
  -> Append_result.t Or_error.t

(** {!Hold.place} stamped and booked. With [timeout], the hold expires that long after it
    is recorded. *)
val hold
  :  t
  -> hold_id:Hold_id.t
  -> account:Account_id.t
  -> amount:Money.Amount.t
  -> ?timeout:Time_ns.Span.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> ?remittance_info:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

val release
  :  t
  -> hold_id:Hold_id.t
  -> ?amount:Money.Amount.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

val capture
  :  t
  -> hold_id:Hold_id.t
  -> ?amount:Money.Amount.t
  -> ?release_remainder:bool
  -> to_:Account_id.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> ?remittance_info:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

val increment
  :  t
  -> hold_id:Hold_id.t
  -> amount:Money.Amount.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

(** Several captures, each a payee and an amount, in one entry. *)
val capture_split
  :  t
  -> hold_id:Hold_id.t
  -> ?release_remainder:bool
  -> (Account_id.t * Money.Amount.t) list
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> ?remittance_info:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

val over_capture
  :  t
  -> hold_id:Hold_id.t
  -> amount:Money.Amount.t
  -> reason:string
  -> to_:Account_id.t
  -> idempotency_key:Idempotency_key.t
  -> ?code:Entry.Code.t
  -> ?sca:Sca_evidence.t
  -> ?reference:string
  -> ?remittance_info:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

(** {!Reversal.create} stamped and booked. *)
val reverse
  :  t
  -> reverses:Idempotency_key.t
  -> idempotency_key:Idempotency_key.t
  -> ?reference:string
  -> effective_at:Time_ns.t
  -> unit
  -> Append_result.t Or_error.t

(** {2 Hold expiry}

    Nothing expires a hold by itself: an expired hold refuses captures and releases, but
    its funds stay held until an expiry is booked. {!close_day} books the due ones;
    between closes, a caller schedules {!expire_holds_at} for {!next_expiry} on whatever
    timer it has. With Async, for example:

    {[
      let rec schedule journal =
        Option.iter (Journal.next_expiry journal) ~f:(fun at ->
          Clock_ns.run_at
            at
            (fun () ->
              ignore (Journal.expire_holds_at journal at : _ Or_error.t);
              schedule journal)
            ())
      ;;
    ]}

    (Reschedule after placing a hold that expires earlier than the pending timer.) *)

(** The earliest expiry among the open holds, if any hold has one. *)
val next_expiry : t -> Time_ns.t option

(** Books an expiry for every open hold whose [expires_at] is at or before [time], under
    the key [expire:<hold id>], in hold id order. Each expiry is recorded at the journal's
    {!now}, or at the hold's [expires_at] if that is later (so a [time] ahead of the
    journal's clock moves ledger time forward to that expiry). Already expired holds are
    skipped, so calling it twice books nothing new. *)
val expire_holds_at : t -> Time_ns.t -> Append_result.t list Or_error.t

(** [expire_holds_at t (now t)]. *)
val expire_holds : t -> Append_result.t list Or_error.t

(** {2 Adjustments (four eyes)} *)

(** Records [entry] (built with any code; restamped) as a proposed correction by [maker].
    Nothing is booked until another operator approves it within
    {!Balances.proposal_lifetime}. *)
val propose_adjustment
  :  t
  -> Entry.t
  -> maker:string
  -> reason:string
  -> ticket:string
  -> Append_result.t Or_error.t

(** Books the proposal under [idempotency_key] with code
    [Adjustment { maker; checker; reason; ticket }]. Fails if [checker] is the maker, the
    proposal expired or was decided, or the entry is refused by the ledger rules (the
    proposal then stays open). *)
val approve_adjustment
  :  t
  -> idempotency_key:Idempotency_key.t
  -> checker:string
  -> Append_result.t Or_error.t

val reject_adjustment
  :  t
  -> idempotency_key:Idempotency_key.t
  -> checker:string
  -> Append_result.t Or_error.t

(** {2 End of day} *)

(** Expires due holds, appends a [Day_closed] record for [business_date] (default: the
    current business date), writes a {!Snapshot} to [snapshot_path] (default
    {!Snapshot.default_path}) and returns it. Later entries are booked on a later business
    date. *)
val close_day
  :  ?business_date:Date.t
  -> ?snapshot_path:string
  -> t
  -> Snapshot.t Or_error.t

(** {2 Reading without a lock} *)

(** Folds [f] over the records of a journal file in sequence order, reading one frame at a
    time, and returns the result, the balances they fold to and what follows the last
    intact frame. Fails like {!read}. Memory is the state, not the file: use it for
    reports over large journals (filter the records one statement needs, then call
    {!Statement.create} on those). *)
val fold
  :  string
  -> init:'acc
  -> f:('acc -> Record.t -> 'acc)
  -> ('acc * Balances.t * Framing.Tail.t) Or_error.t

(** The records of a journal file and what follows the last intact frame. Fails on a bad
    header or a record that breaks a ledger rule. Holds every record in memory; see
    {!fold} for a large file. *)
val read : string -> (Record.t list * Framing.Tail.t) Or_error.t

(** Folds a journal file to its balances. Fails like {!read}, and on a corrupt (but not a
    truncated) tail. *)
val replay : string -> Balances.t Or_error.t
