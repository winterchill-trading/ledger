open! Core
module Time_source = Async_kernel.Time_source

module Append_result = struct
  type t =
    | Appended of Record.t
    | Duplicate of { seq : int }
  [@@deriving sexp_of, compare, equal]

  let seq = function
    | Appended r -> r.seq
    | Duplicate { seq } -> seq
  ;;
end

type t =
  { path : string
  ; fd : Core_unix.File_descr.t
  ; time_source : read Time_source.T1.t
  ; fsync : bool
  ; remember_refusals : bool
  ; mutable length : int (* bytes in the file, header included *)
  ; mutable head : string (* hash of the last frame's payload; the next frame's link *)
  ; mutable balances : Balances.t
  ; mutable records_rev : Record.t list
  ; mutable state : [ `Open | `Failed of Error.t | `Closed ]
  }

let path t = t.path
let balances t = t.balances
let records t = List.rev t.records_rev
let utc_date time = Time_ns.to_date time ~zone:Time_float.Zone.utc

let now t =
  let clock = Time_source.now t.time_source in
  match Balances.last_recorded_at t.balances with
  | Some last when Time_ns.(clock <= last) -> Time_ns.add last Time_ns.Span.nanosecond
  | _ -> clock
;;

let business_date_at t time =
  let today = utc_date time in
  match Balances.closed_through t.balances with
  | Some closed when Date.( <= ) today closed -> Date.add_days closed 1
  | _ -> today
;;

let business_date t = business_date_at t (now t)

let close t =
  match t.state with
  | `Closed -> ()
  | `Open | `Failed _ ->
    t.state <- `Closed;
    (* Closing the descriptor drops the lockf lock. *)
    Core_unix.close t.fd
;;

let lock ~path fd =
  match Core_unix.lockf fd ~mode:F_TLOCK ~len:0L with
  | () -> Ok ()
  | exception Core_unix.Unix_error ((EAGAIN | EACCES), _, _) ->
    Core_unix.close fd;
    Or_error.error_s [%message "journal is open for writing elsewhere" path]
  | exception exn ->
    Core_unix.close fd;
    Or_error.of_exn exn
;;

let write_all fd s =
  let written = Core_unix.write_substring fd ~buf:s in
  if written <> String.length s
  then raise_s [%message "short write" (written : int) ~expected:(String.length s : int)]
;;

let fsync_dir_of path =
  (* A new file is only durable once the directory entry naming it is synced too. *)
  let dir = Core_unix.openfile (Filename.dirname path) ~mode:[ O_RDONLY ] in
  Exn.protect ~f:(fun () -> Core_unix.fsync dir) ~finally:(fun () -> Core_unix.close dir)
;;

let make
  ?time_source
  ?(fsync = true)
  ?(remember_refusals = true)
  ~path
  ~fd
  ~length
  ~head
  ~balances
  ~records_rev
  ()
  =
  let time_source =
    match time_source with
    | Some ts -> Time_source.read_only ts
    | None -> Time_source.read_only (Time_source.wall_clock ())
  in
  { path
  ; fd
  ; time_source
  ; fsync
  ; remember_refusals
  ; length
  ; head
  ; balances
  ; records_rev
  ; state = `Open
  }
;;

let create ?time_source ?fsync ?remember_refusals path =
  Or_error.try_with_join (fun () ->
    let fd =
      Core_unix.openfile path ~mode:[ O_WRONLY; O_CREAT; O_EXCL; O_APPEND ] ~perm:0o644
    in
    let%map.Or_error () = lock ~path fd in
    let header = Framing.header ~digest:Record.digest in
    write_all fd header;
    if Option.value fsync ~default:true
    then (
      Core_unix.fsync fd;
      fsync_dir_of path);
    make
      ?time_source
      ?fsync
      ?remember_refusals
      ~path
      ~fd
      ~length:(String.length header)
      ~head:Framing.genesis
      ~balances:Balances.empty
      ~records_rev:[]
      ())
;;

let decode ?start contents =
  Framing.decode
    ?start
    ~accept:Record.compatible_digests
    Record.Stable.V1.bin_reader_t
    contents
;;

let fold_records ?(init = Balances.empty) records =
  Or_error.tag
    (List.fold_result records ~init ~f:Balances.apply)
    ~tag:"journal breaks a ledger rule"
;;

let read path =
  Or_error.try_with_join (fun () ->
    let%bind.Or_error decoded = decode (In_channel.read_all path) in
    let%map.Or_error _balances = fold_records decoded.records in
    decoded.records, decoded.tail)
  |> Or_error.tag_s ~tag:[%message "Journal.read" path]
;;

let replay path =
  Or_error.try_with_join (fun () ->
    let%bind.Or_error decoded = decode (In_channel.read_all path) in
    match decoded.tail with
    | Corrupt _ ->
      Or_error.error_s [%message "corrupt journal" ~tail:(decoded.tail : Framing.Tail.t)]
    | Clean | Truncated _ -> fold_records decoded.records)
  |> Or_error.tag_s ~tag:[%message "Journal.replay" path]
;;

let open_
  ?time_source
  ?fsync
  ?remember_refusals
  ?(repair_truncated_tail = false)
  ?snapshot
  path
  =
  Or_error.try_with_join (fun () ->
    let fd = Core_unix.openfile path ~mode:[ O_RDWR; O_APPEND ] in
    let%bind.Or_error () = lock ~path fd in
    let result =
      let open Or_error.Let_syntax in
      let contents = In_channel.read_all path in
      let%bind start, init =
        match (snapshot : Snapshot.t option) with
        | None -> Ok (None, Balances.empty)
        | Some s ->
          (* Only the prefix check here; [Snapshot.verify] is the full replay. *)
          let md5 =
            if String.length contents < s.journal_length
            then ""
            else Md5.to_hex (Md5.digest_string (String.prefix contents s.journal_length))
          in
          if String.equal md5 s.journal_md5
          then Ok (Some (s.journal_length, s.journal_head), s.state)
          else Or_error.error_s [%message "snapshot does not match this journal"]
      in
      let%bind decoded = decode ?start contents in
      let%bind () =
        match decoded.tail with
        | Clean -> Ok ()
        | Truncated _ when repair_truncated_tail ->
          Core_unix.ftruncate fd ~len:(Int64.of_int decoded.valid_length);
          Core_unix.fsync fd;
          Ok ()
        | Truncated _ ->
          Or_error.error_s
            [%message
              "journal ends in a partial record (a crash during an append); reopen with \
               repair_truncated_tail to cut it off"
                ~tail:(decoded.tail : Framing.Tail.t)]
        | Corrupt _ ->
          Or_error.error_s
            [%message "corrupt journal" ~tail:(decoded.tail : Framing.Tail.t)]
      in
      let%map balances = fold_records ~init decoded.records in
      make
        ?time_source
        ?fsync
        ?remember_refusals
        ~path
        ~fd
        ~length:decoded.valid_length
        ~head:decoded.head
        ~balances
        ~records_rev:(List.rev decoded.records)
        ()
    in
    if Result.is_error result then Core_unix.close fd;
    result)
  |> Or_error.tag_s ~tag:[%message "Journal.open_" path]
;;

let writable t =
  match t.state with
  | `Open -> Ok ()
  | `Closed -> Or_error.error_s [%message "journal is closed" t.path]
  | `Failed error ->
    Or_error.error_s
      [%message "an earlier write failed; reopen the journal" t.path (error : Error.t)]
;;

(* Validates, writes and syncs one record. [Error] leaves the state unchanged. *)
let write_event t (event : Record.Event.t) =
  let%bind.Or_error () = writable t in
  let record = { Record.seq = Balances.next_seq t.balances; event } in
  let%bind.Or_error balances = Balances.apply t.balances record in
  let frame, hash =
    Framing.encode_frame ~prev:t.head Record.Stable.V1.bin_writer_t record
  in
  match
    write_all t.fd frame;
    if t.fsync then Core_unix.fsync t.fd
  with
  | () ->
    t.balances <- balances;
    t.length <- t.length + String.length frame;
    t.head <- hash;
    t.records_rev <- record :: t.records_rev;
    Ok record
  | exception exn ->
    let error = Error.of_exn exn in
    t.state <- `Failed error;
    Error error
;;

let append_event t event =
  Or_error.map (write_event t event) ~f:(fun r -> Append_result.Appended r)
;;

let open_account t (account : Account.t) =
  let%bind.Or_error () = writable t in
  match Balances.find_account t.balances account.id with
  | Some existing when Account.equal existing account ->
    let seq =
      List.find_map_exn t.records_rev ~f:(fun r ->
        match r.event with
        | Account_opened a when Account_id.equal a.id account.id -> Some r.seq
        | _ -> None)
    in
    Ok (Append_result.Duplicate { seq })
  | Some _ | None -> append_event t (Account_opened account)
;;

let set_status t account status =
  append_event t (Account_status_changed { account; status; recorded_at = now t })
;;

(* Refusals by [Balances.apply] are remembered under each key (see the .mli). Writing the
   refusal can itself fail (the journal was closed); the original error wins. *)
let remember_refusal t (entries : Entry.t list) error =
  if t.remember_refusals
  then
    List.iter entries ~f:(fun e ->
      if Option.is_none (Balances.find_idempotency_key t.balances e.idempotency_key)
         && Option.is_none (Balances.find_rejection t.balances e.idempotency_key)
      then
        ignore
          (write_event
             t
             (Rejected
                { idempotency_key = e.idempotency_key
                ; reason = Error.to_string_mach error
                ; recorded_at = now t
                })
           : Record.t Or_error.t));
  Error error
;;

let stamp t (entries : Entry.t list) =
  let first = now t in
  (* One nanosecond apart, so every entry of a group has its own timestamp. *)
  List.mapi entries ~f:(fun i e ->
    Entry.restamp
      e
      ~recorded_at:(Time_ns.add first (Time_ns.Span.of_int_ns i))
      ~business_date:(business_date_at t first))
;;

(* What to do with entries whose keys may already be known. *)
let classify t (entries : Entry.t list) =
  let known =
    List.filter_map entries ~f:(fun e ->
      match
        ( Balances.find_entry t.balances e.idempotency_key
        , Balances.find_rejection t.balances e.idempotency_key )
      with
      | Some (seq, booked), _ -> Some (`Booked (seq, booked, e))
      | None, Some (seq, reason) -> Some (`Refused (seq, reason, e))
      | None, None -> None)
  in
  match known with
  | [] -> `New
  | `Refused (refused_at, reason, e) :: _ ->
    `Error
      (Error.create_s
         [%message
           "idempotency key already refused"
             ~idempotency_key:(e.idempotency_key : Idempotency_key.t)
             (refused_at : int)
             reason])
  | `Booked (seq, _, _) :: _ ->
    let same =
      List.length known = List.length entries
      && List.for_all known ~f:(function
        | `Booked (s, booked, e) -> s = seq && Entry.same_request booked e
        | `Refused _ -> false)
    in
    if same
    then `Duplicate seq
    else
      `Error
        (Error.create_s
           [%message
             "idempotency key already used for a different entry"
               ~idempotency_keys:
                 (List.map entries ~f:(fun e -> e.Entry.idempotency_key)
                  : Idempotency_key.t list)
               ~booked_at:(seq : int)])
;;

let book_entries t entries ~event =
  let%bind.Or_error () = writable t in
  match classify t entries with
  | `Duplicate seq -> Ok (Append_result.Duplicate { seq })
  | `Error e -> Error e
  | `New ->
    let stamped = stamp t entries in
    (match write_event t (event stamped) with
     | Ok r -> Ok (Append_result.Appended r)
     | Error e -> remember_refusal t stamped e)
;;

let append_entry t entry =
  book_entries t [ entry ] ~event:(function
    | [ e ] -> Record.Event.Posted e
    | es -> Linked es)
;;

let append_linked t entries =
  match entries with
  | [] -> Or_error.error_s [%message "empty linked group"]
  | _ -> book_entries t entries ~event:(fun es -> Linked es)
;;

(* A builder that fails because the hold was resolved by the first attempt of this very
   request is a retry, not an error. *)
let book_built t ~idempotency_key ~(kind : Entry.Kind.t) built =
  let%bind.Or_error () = writable t in
  match built, Balances.find_entry t.balances idempotency_key with
  | Error _, Some (seq, booked) when Entry.Kind.equal booked.kind kind ->
    Ok (Append_result.Duplicate { seq })
  | Error e, _ -> Error e
  | Ok entry, _ -> append_entry t entry
;;

let transfer
  t
  ~idempotency_key
  ?(tag = "transfer")
  ?code
  ?reference
  ?remittance_info
  ~effective_at
  postings
  =
  let kind = Entry.Kind.Transfer tag in
  book_built
    t
    ~idempotency_key
    ~kind
    (Entry.create
       ~idempotency_key
       ~kind
       ?code
       ?reference
       ?remittance_info
       ~effective_at
       ~recorded_at:(now t)
       postings)
;;

let hold
  t
  ~hold_id
  ~account
  ~amount
  ?timeout
  ~idempotency_key
  ?code
  ?reference
  ?remittance_info
  ~effective_at
  ()
  =
  let recorded_at = now t in
  let expires_at = Option.map timeout ~f:(Time_ns.add recorded_at) in
  let built =
    Hold.place
      t.balances
      ~hold_id
      ~account
      ~amount
      ?expires_at
      ~idempotency_key
      ?code
      ?reference
      ?remittance_info
      ~effective_at
      ~recorded_at
      ()
  in
  (* The expiry is computed from the clock, so a retry builds a different time; match
     retries on the hold id instead. *)
  match Balances.find_entry t.balances idempotency_key with
  | Some (seq, booked)
    when Option.equal Hold_id.equal (Entry.Kind.hold booked.kind) (Some hold_id) ->
    Ok (Append_result.Duplicate { seq })
  | _ -> book_built t ~idempotency_key ~kind:(Hold_place hold_id) built
;;

let release t ~hold_id ?amount ~idempotency_key ?code ?reference ~effective_at () =
  book_built
    t
    ~idempotency_key
    ~kind:(Hold_release hold_id)
    (Hold.release
       t.balances
       ~hold_id
       ?amount
       ~idempotency_key
       ?code
       ?reference
       ~effective_at
       ~recorded_at:(now t)
       ())
;;

let capture
  t
  ~hold_id
  ?amount
  ?release_remainder
  ~to_
  ~idempotency_key
  ?code
  ?reference
  ?remittance_info
  ~effective_at
  ()
  =
  book_built
    t
    ~idempotency_key
    ~kind:(Hold_capture hold_id)
    (Hold.capture
       t.balances
       ~hold_id
       ?amount
       ?release_remainder
       ~to_
       ~idempotency_key
       ?code
       ?reference
       ?remittance_info
       ~effective_at
       ~recorded_at:(now t)
       ())
;;

let increment t ~hold_id ~amount ~idempotency_key ?code ?reference ~effective_at () =
  book_built
    t
    ~idempotency_key
    ~kind:(Hold_increment hold_id)
    (Hold.increment
       t.balances
       ~hold_id
       ~amount
       ~idempotency_key
       ?code
       ?reference
       ~effective_at
       ~recorded_at:(now t)
       ())
;;

let capture_split
  t
  ~hold_id
  ?release_remainder
  payees
  ~idempotency_key
  ?code
  ?reference
  ?remittance_info
  ~effective_at
  ()
  =
  book_built
    t
    ~idempotency_key
    ~kind:(Hold_capture hold_id)
    (Hold.capture_split
       t.balances
       ~hold_id
       ?release_remainder
       payees
       ~idempotency_key
       ?code
       ?reference
       ?remittance_info
       ~effective_at
       ~recorded_at:(now t)
       ())
;;

let over_capture
  t
  ~hold_id
  ~amount
  ~reason
  ~to_
  ~idempotency_key
  ?code
  ?reference
  ?remittance_info
  ~effective_at
  ()
  =
  book_built
    t
    ~idempotency_key
    ~kind:(Hold_over_capture { hold = hold_id; reason })
    (Hold.over_capture
       t.balances
       ~hold_id
       ~amount
       ~reason
       ~to_
       ~idempotency_key
       ?code
       ?reference
       ?remittance_info
       ~effective_at
       ~recorded_at:(now t)
       ())
;;

let reverse t ~reverses ~idempotency_key ?reference ~effective_at () =
  book_built
    t
    ~idempotency_key
    ~kind:(Reversal { reverses })
    (Reversal.create
       t.balances
       ~reverses
       ~idempotency_key
       ?reference
       ~effective_at
       ~recorded_at:(now t)
       ())
;;

let expire_holds t =
  let now = now t in
  Balances.expired_holds t.balances ~now
  |> List.map ~f:(fun (h : Balances.Hold_state.t) ->
    let idempotency_key =
      Idempotency_key.of_string ("expire:" ^ Hold_id.to_string h.id)
    in
    let%bind.Or_error entry =
      Hold.expire
        t.balances
        ~hold_id:h.id
        ~idempotency_key
        ~effective_at:now
        ~recorded_at:(Time_ns.max now (Option.value_exn h.expires_at))
        ()
    in
    append_entry t entry)
  |> Or_error.all
;;

let stamp_one t entry = List.hd_exn (stamp t [ entry ])

let propose_adjustment t entry ~maker ~reason ~ticket =
  append_event
    t
    (Adjustment_proposed { entry = stamp_one t entry; maker; reason; ticket })
;;

let approve_adjustment t ~idempotency_key ~checker =
  match Balances.find_proposal t.balances idempotency_key with
  | None ->
    Or_error.error_s [%message "no such proposal" (idempotency_key : Idempotency_key.t)]
  | Some p ->
    let%bind.Or_error entry =
      Entry.with_code
        p.entry
        (Adjustment { maker = p.maker; checker; reason = p.reason; ticket = p.ticket })
    in
    append_event t (Adjustment_approved (stamp_one t entry))
;;

let reject_adjustment t ~idempotency_key ~checker =
  append_event t (Adjustment_rejected { idempotency_key; checker; recorded_at = now t })
;;

let close_day ?business_date ?snapshot_path t =
  let open Or_error.Let_syntax in
  let%bind _expired = expire_holds t in
  let business_date =
    match business_date with
    | Some d -> d
    | None -> business_date_at t (now t)
  in
  let%bind record = write_event t (Day_closed { business_date; recorded_at = now t }) in
  let snapshot : Snapshot.t =
    { business_date
    ; seq = record.seq
    ; journal_length = t.length
    ; journal_md5 =
        Md5.to_hex
          (Md5.digest_string (String.prefix (In_channel.read_all t.path) t.length))
    ; journal_head = t.head
    ; rows = Balances.day_rows t.balances
    ; state = t.balances
    }
  in
  let path =
    Option.value
      snapshot_path
      ~default:(Snapshot.default_path ~journal:t.path business_date)
  in
  let%map () = Snapshot.write snapshot path in
  snapshot
;;
