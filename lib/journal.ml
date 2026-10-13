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
  ; mutable balances : Balances.t
  ; mutable records_rev : Record.t list
  ; mutable state : [ `Open | `Failed of Error.t | `Closed ]
  }

let path t = t.path
let balances t = t.balances
let records t = List.rev t.records_rev
let now t = Time_source.now t.time_source

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

let make ?time_source ?(fsync = true) ~path ~fd ~balances ~records_rev () =
  let time_source =
    match time_source with
    | Some ts -> Time_source.read_only ts
    | None -> Time_source.wall_clock ()
  in
  { path; fd; time_source; fsync; balances; records_rev; state = `Open }
;;

let create ?time_source ?fsync path =
  Or_error.try_with_join (fun () ->
    let fd =
      Core_unix.openfile path ~mode:[ O_WRONLY; O_CREAT; O_EXCL; O_APPEND ] ~perm:0o644
    in
    let%map.Or_error () = lock ~path fd in
    write_all fd (Framing.header ~digest:Record.digest);
    if Option.value fsync ~default:true
    then (
      Core_unix.fsync fd;
      fsync_dir_of path);
    make ?time_source ?fsync ~path ~fd ~balances:Balances.empty ~records_rev:[] ())
;;

let decode contents =
  Framing.decode ~accept:Record.compatible_digests Record.Stable.V1.bin_reader_t contents
;;

let fold_records records =
  Or_error.tag (Balances.of_records records) ~tag:"journal breaks a ledger rule"
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

let open_ ?time_source ?fsync ?(repair_truncated_tail = false) path =
  Or_error.try_with_join (fun () ->
    let fd = Core_unix.openfile path ~mode:[ O_RDWR; O_APPEND ] in
    let%bind.Or_error () = lock ~path fd in
    let result =
      let%bind.Or_error decoded = decode (In_channel.read_all path) in
      let%bind.Or_error () =
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
      let%map.Or_error balances = fold_records decoded.records in
      make
        ?time_source
        ?fsync
        ~path
        ~fd
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

let append_event t (event : Record.Event.t) =
  match writable t with
  | Error _ as e -> e
  | Ok () ->
    let record = { Record.seq = Balances.next_seq t.balances; event } in
    let%bind.Or_error balances = Balances.apply t.balances record in
    (match
       write_all t.fd (Framing.encode_frame Record.Stable.V1.bin_writer_t record);
       if t.fsync then Core_unix.fsync t.fd
     with
     | () ->
       t.balances <- balances;
       t.records_rev <- record :: t.records_rev;
       Ok (Append_result.Appended record)
     | exception exn ->
       let error = Error.of_exn exn in
       t.state <- `Failed error;
       Error error)
;;

let open_account t (account : Account.t) =
  let%bind.Or_error () = writable t in
  match Balances.find_account t.balances account.id with
  | Some existing when Account.equal existing account ->
    let seq =
      List.find_map_exn t.records_rev ~f:(fun r ->
        match r.event with
        | Account_opened a when Account_id.equal a.id account.id -> Some r.seq
        | Account_opened _ | Posted _ -> None)
    in
    Ok (Append_result.Duplicate { seq })
  | Some _ | None -> append_event t (Account_opened account)
;;

(* A request whose idempotency key is booked already is a retry when it builds the same
   entry, or when it can no longer be built (a hold that the first attempt already
   released) but asks for the same kind of entry. Anything else reuses the key for a
   different entry. *)
let book t ~idempotency_key ~(kind : Entry.Kind.t) (built : Entry.t Or_error.t) =
  let%bind.Or_error () = writable t in
  match Balances.find_idempotency_key t.balances idempotency_key with
  | None ->
    let%bind.Or_error entry = built in
    append_event t (Posted entry)
  | Some seq ->
    let booked = Option.value_exn (Balances.find_entry t.balances ~seq) in
    let is_retry =
      match built with
      | Ok entry -> Entry.same_request booked entry
      | Error _ -> Entry.Kind.equal booked.kind kind
    in
    if is_retry
    then Ok (Append_result.Duplicate { seq })
    else
      Or_error.error_s
        [%message
          "idempotency key already used for a different entry"
            (idempotency_key : Idempotency_key.t)
            ~booked_at:(seq : int)]
;;

let append_entry t (entry : Entry.t) =
  book t ~idempotency_key:entry.idempotency_key ~kind:entry.kind (Ok entry)
;;

let transfer t ~idempotency_key ?(tag = "transfer") ?reference ~effective_at postings =
  let kind = Entry.Kind.Transfer tag in
  book
    t
    ~idempotency_key
    ~kind
    (Entry.create
       ~idempotency_key
       ~kind
       ?reference
       ~effective_at
       ~recorded_at:(now t)
       postings)
;;

let hold t ~hold_id ~account ~amount ~idempotency_key ?reference ~effective_at () =
  book
    t
    ~idempotency_key
    ~kind:(Hold_place hold_id)
    (Hold.place
       t.balances
       ~hold_id
       ~account
       ~amount
       ~idempotency_key
       ?reference
       ~effective_at
       ~recorded_at:(now t)
       ())
;;

let release t ~hold_id ?amount ~idempotency_key ?reference ~effective_at () =
  book
    t
    ~idempotency_key
    ~kind:(Hold_release hold_id)
    (Hold.release
       t.balances
       ~hold_id
       ?amount
       ~idempotency_key
       ?reference
       ~effective_at
       ~recorded_at:(now t)
       ())
;;

let capture t ~hold_id ?amount ~to_ ~idempotency_key ?reference ~effective_at () =
  book
    t
    ~idempotency_key
    ~kind:(Hold_capture hold_id)
    (Hold.capture
       t.balances
       ~hold_id
       ?amount
       ~to_
       ~idempotency_key
       ?reference
       ~effective_at
       ~recorded_at:(now t)
       ())
;;

let reverse t ~seq ~idempotency_key ?reference ~effective_at () =
  book
    t
    ~idempotency_key
    ~kind:(Reversal { reverses = seq })
    (Reversal.create
       t.balances
       ~seq
       ~idempotency_key
       ?reference
       ~effective_at
       ~recorded_at:(now t)
       ())
;;
