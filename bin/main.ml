open! Core
open Ledger

let journal_flag =
  Command.Param.(
    flag "-journal" (required Filename_unix.arg_type) ~doc:"FILE journal file")
;;

let read_or_exit path =
  match Journal.read path with
  | Error e ->
    eprint_s [%sexp (e : Error.t)];
    exit 1
  | Ok (records, tail) ->
    (match tail with
     | Clean -> ()
     | Truncated _ | Corrupt _ ->
       (* Reading is still useful on a damaged journal: show what is intact and say where
          it stops. *)
       eprint_s [%message "warning: journal does not end cleanly" (tail : Framing.Tail.t)]);
    records
;;

let warn_tail (tail : Framing.Tail.t) =
  match tail with
  | Clean -> ()
  | Truncated _ | Corrupt _ ->
    eprint_s [%message "warning: journal does not end cleanly" (tail : Framing.Tail.t)]
;;

(* Streams the file: memory is the ledger state plus what [f] keeps. *)
let fold_or_exit path ~init ~f =
  match Journal.fold path ~init ~f with
  | Error e ->
    eprint_s [%sexp (e : Error.t)];
    exit 1
  | Ok (acc, balances, tail) ->
    warn_tail tail;
    acc, balances
;;

let time_arg =
  Command.Arg_type.create (fun s ->
    match Time_ns.of_string_with_utc_offset s with
    | t -> t
    | exception _ ->
      Time_ns.of_date_ofday
        ~zone:Time_float.Zone.utc
        (Date.of_string s)
        Time_ns.Ofday.start_of_day)
;;

let trial_balance =
  Command.basic
    ~summary:"Print the trial balance of a journal; exit 2 if it does not balance"
    (let%map_open.Command path = journal_flag in
     fun () ->
       let (), balances = fold_or_exit path ~init:() ~f:(fun () _ -> ()) in
       let tb = Trial_balance.of_balances balances in
       print_string (Trial_balance.to_string_hum tb);
       match Trial_balance.check tb with
       | Ok () -> ()
       | Error e ->
         eprint_s [%sexp (e : Error.t)];
         exit 2)
;;

let statement =
  Command.basic
    ~summary:"Print the statement of one account in one currency"
    (let%map_open.Command path = journal_flag
     and account =
       flag "-account" (required (Arg_type.create Account_id.of_string)) ~doc:"ID account"
     and currency =
       flag
         "-currency"
         (optional_with_default
            Money.Currency.eur
            (Arg_type.create Money.Currency.of_string))
         ~doc:"CODE currency (default EUR)"
     and from =
       flag
         "-from"
         (optional time_arg)
         ~doc:"TIME first effective time (inclusive, UTC date or time)"
     and until =
       flag "-until" (optional time_arg) ~doc:"TIME last effective time (exclusive)"
     and after_seq =
       flag "-after" (optional int) ~doc:"SEQ start after this sequence number (paging)"
     and limit = flag "-limit" (optional int) ~doc:"N at most N entries" in
     fun () ->
       (* Keep only the records a statement of [account] reads: account openings and
          entries touching it, its pending sub-account or its parent. *)
       let related =
         Account_id.(
           account :: pending account :: Option.to_list (parent_of_pending account))
       in
       let relevant (r : Record.t) =
         match r.event with
         | Account_opened _ -> true
         | _ ->
           List.exists (Record.entries r) ~f:(fun e ->
             List.exists e.postings ~f:(fun p ->
               List.mem related p.account ~equal:Account_id.equal))
       in
       let records_rev, _ =
         fold_or_exit path ~init:[] ~f:(fun acc r -> if relevant r then r :: acc else acc)
       in
       match
         Statement.create
           ?from
           ?until
           ?after_seq
           ?limit
           (List.rev records_rev)
           ~account
           ~currency
       with
       | Error e ->
         eprint_s [%sexp (e : Error.t)];
         exit 1
       | Ok s -> print_string (Statement.to_string_hum s))
;;

let date_flag name ~doc =
  Command.Param.(flag name (required (Arg_type.create Date.of_string)) ~doc)
;;

let camt053 =
  Command.basic
    ~summary:"Print a camt.053-shaped statement (booked and, optionally, pending entries)"
    (let%map_open.Command path = journal_flag
     and account =
       flag "-account" (required (Arg_type.create Account_id.of_string)) ~doc:"ID account"
     and currency =
       flag
         "-currency"
         (optional_with_default
            Money.Currency.eur
            (Arg_type.create Money.Currency.of_string))
         ~doc:"CODE currency (default EUR)"
     and from_date = date_flag "-from" ~doc:"DATE first business date"
     and to_date = date_flag "-to" ~doc:"DATE last business date"
     and include_pending =
       flag "-pending" no_arg ~doc:" list open holds as PDNG entries"
     in
     fun () ->
       let records = read_or_exit path in
       match
         Camt053.create
           ~include_pending
           records
           ~account
           ~currency
           ~from_date
           ~to_date
           ~created_at:(Time_ns.now ())
       with
       | Error e ->
         eprint_s [%sexp (e : Error.t)];
         exit 1
       | Ok s ->
         (* The creation time is the only thing that changes between runs. *)
         print_string (Camt053.to_string_hum s))
;;

let verify =
  Command.basic
    ~summary:
      "Check a journal end to end (frames, hash chain, ledger rules) and print its chain \
       head; with -snapshot, also check the snapshot against it; with -head, check that \
       the journal still contains a head published earlier"
    (let%map_open.Command path = journal_flag
     and snapshot =
       flag "-snapshot" (optional Filename_unix.arg_type) ~doc:"FILE end-of-day snapshot"
     and published =
       flag
         "-head"
         (optional (Arg_type.create (Fn.compose Or_error.ok_exn Chain_head.of_string)))
         ~doc:"HEAD a chain head printed earlier (ledger-head-v1:...)"
     in
     fun () ->
       let fail e =
         eprint_s [%sexp (e : Error.t)];
         exit 1
       in
       (* Two streaming passes: the ledger rules, then the chain head. *)
       (match
          let%bind.Or_error records, _balances, tail =
            Journal.fold path ~init:0 ~f:(fun n _ -> n + 1)
          in
          let%map.Or_error head = Chain_head.of_file path in
          records, tail, head
        with
        | Error e -> fail e
        | Ok (records, tail, head) ->
          print_s
            [%message
              ""
                (records : int)
                (tail : Framing.Tail.t)
                ~head:head.sha256
                ~chain_head:(Chain_head.to_string head)];
          (match tail with
           | Clean -> ()
           | Truncated _ | Corrupt _ -> exit 2));
       Option.iter published ~f:(fun head ->
         match Chain_head.verify head ~journal:path with
         | Error e -> fail e
         | Ok () -> print_endline "head ok");
       Option.iter snapshot ~f:(fun snapshot ->
         match
           Or_error.bind (Snapshot.read snapshot) ~f:(Snapshot.verify ~journal:path)
         with
         | Error e -> fail e
         | Ok () -> print_endline "snapshot ok"))
;;

(* A small journal to try the other commands on. *)
let demo =
  Command.basic
    ~summary:"Write a small example journal (a deposit, a card payment, a fee)"
    (let%map_open.Command path = journal_flag in
     fun () ->
       let eur s =
         Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn
       in
       let id = Account_id.of_string in
       let key = Idempotency_key.of_string in
       let at = Time_ns.of_string_with_utc_offset in
       (* A fixed clock, so the example has the same dates on every run. *)
       let clock = Async_kernel.Time_source.create ~now:(at "2026-10-01 09:00:00Z") () in
       let j =
         match Journal.create ~time_source:clock path with
         | Ok j -> j
         | Error e ->
           eprint_s [%sexp (e : Error.t)];
           exit 1
       in
       let ok r = ignore (Or_error.ok_exn r : Journal.Append_result.t) in
       List.iter
         [ Account.create (id "bank:central") Asset ~purpose:Safeguarding_asset
         ; Account.create (id "external:cards") Asset ~allow_negative:true
         ; Account.create (id "income:fees") Income ~purpose:Fee_income
         ; Account.create (id "customer:alice") Liability ~purpose:Customer_funds
         ]
         ~f:(fun a -> ok (Journal.open_account j a));
       ok
         (Journal.transfer
            j
            ~idempotency_key:(key "dep-1")
            ~tag:"deposit"
            ~code:Deposit
            ~reference:"sepa-in-001"
            ~effective_at:(at "2026-10-01 09:00:00Z")
            [ Posting.debit (id "bank:central") (eur "250.00")
            ; Posting.credit (id "customer:alice") (eur "250.00")
            ]);
       ok
         (Journal.hold
            j
            ~hold_id:(Hold_id.of_string "auth-1")
            ~account:(id "customer:alice")
            ~amount:(eur "18.40")
            ~idempotency_key:(key "auth-1")
            ~code:Card_auth
            ~reference:"card-bakery"
            ~effective_at:(at "2026-10-02 08:15:00Z")
            ());
       ok
         (Journal.capture
            j
            ~hold_id:(Hold_id.of_string "auth-1")
            ~to_:(id "external:cards")
            ~idempotency_key:(key "cap-1")
            ~code:Card_settlement
            ~reference:"card-bakery"
            ~effective_at:(at "2026-10-03 02:00:00Z")
            ());
       ok
         (Journal.transfer
            j
            ~idempotency_key:(key "fee-2026-10")
            ~tag:"fee"
            ~code:Fee
            ~reference:"monthly-fee"
            ~effective_at:(at "2026-10-31 23:00:00Z")
            [ Posting.debit (id "customer:alice") (eur "1.50")
            ; Posting.credit (id "income:fees") (eur "1.50")
            ]);
       Journal.close j;
       printf "wrote %s\n" path)
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Inspect ledger journals"
       [ "trial-balance", trial_balance
       ; "statement", statement
       ; "camt053", camt053
       ; "verify", verify
       ; "demo", demo
       ])
;;
