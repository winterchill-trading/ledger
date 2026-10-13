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
       let records = read_or_exit path in
       match Balances.of_records records with
       | Error e ->
         eprint_s [%sexp (e : Error.t)];
         exit 1
       | Ok balances ->
         let tb = Trial_balance.of_balances balances in
         print_string (Trial_balance.to_string_hum tb);
         (match Trial_balance.check tb with
          | Ok () -> ()
          | Error e ->
            eprint_s [%sexp (e : Error.t)];
            exit 2))
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
       let records = read_or_exit path in
       match
         Statement.create ?from ?until ?after_seq ?limit records ~account ~currency
       with
       | Error e ->
         eprint_s [%sexp (e : Error.t)];
         exit 1
       | Ok s -> print_string (Statement.to_string_hum s))
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
       let j =
         match Journal.create path with
         | Ok j -> j
         | Error e ->
           eprint_s [%sexp (e : Error.t)];
           exit 1
       in
       let ok r = ignore (Or_error.ok_exn r : Journal.Append_result.t) in
       List.iter
         [ Account.create (id "bank:central") Asset
         ; Account.create (id "external:cards") Asset ~allow_negative:true
         ; Account.create (id "income:fees") Income
         ; Account.create (id "customer:alice") Liability
         ]
         ~f:(fun a -> ok (Journal.open_account j a));
       ok
         (Journal.transfer
            j
            ~idempotency_key:(key "dep-1")
            ~tag:"deposit"
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
            ~reference:"card-bakery"
            ~effective_at:(at "2026-10-02 08:15:00Z")
            ());
       ok
         (Journal.capture
            j
            ~hold_id:(Hold_id.of_string "auth-1")
            ~to_:(id "external:cards")
            ~idempotency_key:(key "cap-1")
            ~reference:"card-bakery"
            ~effective_at:(at "2026-10-03 02:00:00Z")
            ());
       ok
         (Journal.transfer
            j
            ~idempotency_key:(key "fee-2026-10")
            ~tag:"fee"
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
       [ "trial-balance", trial_balance; "statement", statement; "demo", demo ])
;;
