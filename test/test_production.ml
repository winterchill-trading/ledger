(* The production-ledger behaviours: linked groups, remembered refusals, ledger time,
   account status, hold expiry and partial/over-captures, four-eyes adjustments, day close
   with snapshots, camt.053 statements, clearing and safeguarding reports. *)

open! Core
open Ledger
open Helpers
module Time_source = Async_kernel.Time_source

let eur_ = Money.Currency.eur
let with_dir = Test_journal.with_dir

(* A bank with purposes set, on a synthetic clock that tests move by hand. *)
let fx = id "clearing:fx"
let fx_btc = id "clearing:fx-btc"
let safeguarding = id "bank:safeguarding"
let sepa_out = id "clearing:sepa_out"

let bank () =
  [ Account.create safeguarding Asset ~purpose:Safeguarding_asset
  ; Account.create network Asset ~allow_negative:true
  ; Account.create card_scheme Asset ~allow_negative:true
  ; Account.create fees Income ~purpose:Fee_income
  ; Account.create fx Asset ~purpose:Clearing ~allow_negative:true
  ; Account.create fx_btc Asset ~purpose:Clearing ~allow_negative:true
  ; Account.create sepa_out Asset ~purpose:Clearing ~allow_negative:true
  ; Account.create alice Liability ~purpose:Customer_funds
  ; Account.create bob Liability ~purpose:Customer_funds
  ]
;;

let setup ?(start = at 0) dir =
  let clock = Time_source.create ~now:start () in
  let j = Journal.create ~fsync:false ~time_source:clock (dir ^/ "j.bin") |> ok_exn in
  List.iter (bank ()) ~f:(fun a ->
    ignore (Journal.open_account j a |> ok_exn : Journal.Append_result.t));
  let deposit ?(k = "dep-1") account amount =
    Journal.transfer
      j
      ~idempotency_key:(key k)
      ~code:Deposit
      ~reference:("e2e-" ^ k)
      ~remittance_info:"salary"
      ~effective_at:(Journal.now j)
      [ Posting.debit safeguarding (eur amount); Posting.credit account (eur amount) ]
    |> ok_exn
    |> (ignore : Journal.Append_result.t -> unit)
  in
  clock, j, deposit
;;

let show r =
  print_s [%sexp (Or_error.map r ~f:Journal.Append_result.seq : int Or_error.t)]
;;

let bal j account = show_balance (Journal.balances j) account eur_

let%expect_test "linked entries book together or not at all" =
  with_dir (fun dir ->
    let _, j, deposit = setup dir in
    deposit alice "100.00";
    let now = Journal.now j in
    let leg k postings = entry k ~at:now postings |> ok_exn in
    let swap ~fee =
      [ leg "fx-fee" [ Posting.debit alice (eur fee); Posting.credit fees (eur fee) ]
      ; leg
          "fx-eur"
          [ Posting.debit alice (eur "90.00"); Posting.credit fx (eur "90.00") ]
      ]
    in
    (* 90 + 20 is more than Alice has: neither leg books, and the fee key is remembered as
       refused. *)
    print_error ~dir (Journal.append_linked j (swap ~fee:"20.00"));
    bal j alice;
    print_s
      [%sexp
        (Balances.find_rejection (Journal.balances j) (key "fx-fee")
         : (int * string) option)];
    (* New keys for a new attempt. *)
    let ok =
      [ leg
          "fx-fee-2"
          [ Posting.debit alice (eur "1.00"); Posting.credit fees (eur "1.00") ]
      ; leg
          "fx-eur-2"
          [ Posting.debit alice (eur "90.00"); Posting.credit fx (eur "90.00") ]
      ]
    in
    show (Journal.append_linked j ok);
    show (Journal.append_linked j ok);
    bal j alice);
  [%expect
    {|
    (("linked entry refused, so the whole group is" (idempotency_key fx-eur))
     ("entry would take an account below zero" (id customer:alice)
      (balance_after "-10.00 EUR") (idempotency_key fx-eur)))
    (customer:alice (available "100.00 EUR") (held "0.00 EUR")
     (posted "100.00 EUR"))
    ((11
      "((\"linked entry refused, so the whole group is\"(idempotency_key fx-eur))(\"entry would take an account below zero\"(id customer:alice)(balance_after\"-10.00 EUR\")(idempotency_key fx-eur)))"))
    (Ok 13)
    (Ok 13)
    (customer:alice (available "9.00 EUR") (held "0.00 EUR") (posted "9.00 EUR"))
    |}]
;;

let%expect_test "a refused key stays refused" =
  with_dir (fun dir ->
    let _, j, deposit = setup dir in
    let pay () =
      Journal.transfer
        j
        ~idempotency_key:(key "pay-1")
        ~effective_at:(Journal.now j)
        [ Posting.debit alice (eur "10.00"); Posting.credit bob (eur "10.00") ]
    in
    print_error ~dir (pay ());
    deposit alice "50.00";
    (* Funds are there now, but the answer to request pay-1 does not change. *)
    print_error ~dir (pay ()));
  [%expect
    {|
    ("entry would take an account below zero" (id customer:alice)
     (balance_after "-10.00 EUR") (idempotency_key pay-1))
    ("idempotency key already refused" (idempotency_key pay-1) (refused_at 10)
     "(\"entry would take an account below zero\"(id customer:alice)(balance_after\"-10.00 EUR\")(idempotency_key pay-1))")
    |}]
;;

let%expect_test "the ledger assigns strictly increasing time and business dates" =
  with_dir (fun dir ->
    let clock, j, deposit = setup dir in
    deposit alice "10.00" ~k:"a";
    (* The clock steps back an hour: timestamps still increase. *)
    Time_source.advance_directly clock ~to_:(at (-60));
    deposit alice "10.00" ~k:"b";
    let stamps =
      Journal.records j
      |> List.concat_map ~f:Record.entries
      |> List.map ~f:(fun (e : Entry.t) -> e.recorded_at, e.business_date)
    in
    print_s [%sexp (stamps : (Time_ns.Alternate_sexp.t * Date.t) list)];
    (* After closing 2026-10-01 at 09:00, bookings that day go to 2026-10-02. *)
    let _snapshot = Journal.close_day j |> ok_exn in
    deposit alice "10.00" ~k:"c";
    print_s [%sexp (Journal.business_date j : Date.t)];
    (* An entry dated in the closed period is refused. *)
    let late =
      Entry.create
        ~idempotency_key:(key "late")
        ~kind:(Transfer "x")
        ~business_date:(Date.of_string "2026-10-01")
        ~effective_at:(at 0)
        ~recorded_at:(Time_ns.add (Journal.now j) Time_ns.Span.second)
        [ Posting.debit safeguarding (eur "1.00"); Posting.credit alice (eur "1.00") ]
      |> ok_exn
    in
    print_error
      (Balances.apply
         (Journal.balances j)
         { seq = Balances.next_seq (Journal.balances j); event = Posted late }));
  [%expect
    {|
    (("2026-10-01 09:00:00Z" 2026-10-01)
     ("2026-10-01 09:00:00.000000001Z" 2026-10-01))
    2026-10-02
    ("business date already closed" (entry.business_date 2026-10-01)
     (closed_through 2026-10-01))
    |}]
;;

let%expect_test "frozen and closed accounts" =
  with_dir (fun dir ->
    let _, j, deposit = setup dir in
    deposit alice "100.00";
    ignore
      (Journal.hold
         j
         ~hold_id:(Hold_id.of_string "auth-1")
         ~account:alice
         ~amount:(eur "10.00")
         ~idempotency_key:(key "h1")
         ~effective_at:(Journal.now j)
         ()
       |> ok_exn
       : Journal.Append_result.t);
    show (Journal.set_status j alice Frozen);
    let pay k =
      Journal.transfer
        j
        ~idempotency_key:(key k)
        ~effective_at:(Journal.now j)
        [ Posting.debit alice (eur "1.00"); Posting.credit bob (eur "1.00") ]
    in
    print_error ~dir (pay "out-1");
    (* Money in is fine, and a hold can still be released. *)
    deposit alice "5.00" ~k:"dep-2";
    show
      (Journal.release
         j
         ~hold_id:(Hold_id.of_string "auth-1")
         ~idempotency_key:(key "r1")
         ~effective_at:(Journal.now j)
         ());
    print_error ~dir (Journal.set_status j alice Closed);
    show (Journal.set_status j alice Open);
    show (pay "out-2");
    (* Bob empties his account and closes it for good. *)
    show
      (Journal.transfer
         j
         ~idempotency_key:(key "bob-out")
         ~effective_at:(Journal.now j)
         [ Posting.debit bob (eur "1.00"); Posting.credit network (eur "1.00") ]);
    show (Journal.set_status j bob Closed);
    print_error ~dir (pay "out-3");
    print_error ~dir (Journal.set_status j bob Open));
  [%expect
    {|
    (Ok 12)
    ("account does not accept this entry" (account customer:alice)
     (status Frozen) (idempotency_key out-1))
    (Ok 15)
    ("only an account with nothing booked or held can be closed"
     (account customer:alice) (posted "105.00 EUR") (held "0.00 EUR"))
    (Ok 16)
    (Ok 17)
    (Ok 18)
    (Ok 19)
    ("account does not accept this entry" (account customer:bob) (status Closed)
     (idempotency_key out-3))
    ("account is closed for good" (account customer:bob))
    |}]
;;

let%expect_test "holds: increment, split capture, release of the rest, over-capture, \
                 expiry"
  =
  with_dir (fun dir ->
    let clock, j, deposit = setup dir in
    deposit alice "200.00";
    let h = Hold_id.of_string in
    let now () = Journal.now j in
    let ok r = ignore (ok_exn r : Journal.Append_result.t) in
    (* A hotel: authorised 50, extended by 30, settled at 70 to the scheme with a 1.00
       fee, the rest released in the same entry. *)
    ok
      (Journal.hold
         j
         ~hold_id:(h "hotel")
         ~account:alice
         ~amount:(eur "50.00")
         ~code:Card_auth
         ~idempotency_key:(key "hotel-auth")
         ~effective_at:(now ())
         ());
    ok
      (Journal.increment
         j
         ~hold_id:(h "hotel")
         ~amount:(eur "30.00")
         ~idempotency_key:(key "hotel-inc")
         ~effective_at:(now ())
         ());
    bal j alice;
    ok
      (Journal.capture_split
         j
         ~hold_id:(h "hotel")
         ~release_remainder:true
         [ card_scheme, eur "69.00"; fees, eur "1.00" ]
         ~code:Card_settlement
         ~idempotency_key:(key "hotel-clear")
         ~effective_at:(now ())
         ());
    bal j alice;
    (* A restaurant: authorised 20, cleared at 23 with a tip. *)
    ok
      (Journal.hold
         j
         ~hold_id:(h "dinner")
         ~account:alice
         ~amount:(eur "20.00")
         ~idempotency_key:(key "dinner-auth")
         ~effective_at:(now ())
         ());
    print_error
      (Journal.capture
         j
         ~hold_id:(h "dinner")
         ~amount:(eur "23.00")
         ~to_:card_scheme
         ~idempotency_key:(key "dinner-clear-x")
         ~effective_at:(now ())
         ());
    ok
      (Journal.over_capture
         j
         ~hold_id:(h "dinner")
         ~amount:(eur "23.00")
         ~reason:"tip"
         ~to_:card_scheme
         ~idempotency_key:(key "dinner-clear")
         ~effective_at:(now ())
         ());
    bal j alice;
    (* A 7-day authorisation that nobody clears. *)
    ok
      (Journal.hold
         j
         ~hold_id:(h "fuel")
         ~account:alice
         ~amount:(eur "100.00")
         ~timeout:(Time_ns.Span.of_day 7.)
         ~idempotency_key:(key "fuel-auth")
         ~effective_at:(now ())
         ());
    print_s [%sexp (Journal.expire_holds j |> ok_exn |> List.length : int)];
    Time_source.advance_directly
      clock
      ~to_:(Time_ns.add (now ()) (Time_ns.Span.of_day 8.));
    print_error
      (Journal.capture
         j
         ~hold_id:(h "fuel")
         ~to_:card_scheme
         ~idempotency_key:(key "fuel-clear")
         ~effective_at:(now ())
         ());
    print_s
      [%sexp
        (Journal.expire_holds j |> ok_exn |> List.map ~f:Journal.Append_result.seq
         : int list)];
    bal j alice;
    print_s
      [%sexp (Balances.columns (Journal.balances j) alice eur_ : Balances.Columns.t)];
    Trial_balance.of_balances (Journal.balances j) |> Trial_balance.check |> ok_exn);
  [%expect
    {|
    (customer:alice (available "120.00 EUR") (held "80.00 EUR")
     (posted "200.00 EUR"))
    (customer:alice (available "130.00 EUR") (held "0.00 EUR")
     (posted "130.00 EUR"))
    ("more than the hold's remaining amount" (hold dinner)
     (requested "23.00 EUR") (remaining "20.00 EUR"))
    (customer:alice (available "107.00 EUR") (held "0.00 EUR")
     (posted "107.00 EUR"))
    0
    ("hold has expired" (id fuel)
     (expires_at ("2026-10-08 09:00:00.000000007Z")))
    (19)
    (customer:alice (available "107.00 EUR") (held "0.00 EUR")
     (posted "107.00 EUR"))
    ((debits_posted "93.00 EUR") (credits_posted "200.00 EUR")
     (debits_pending "0.00 EUR") (credits_pending "0.00 EUR"))
    |}]
;;

let%expect_test "adjustments need a second operator" =
  with_dir (fun dir ->
    let clock, j, deposit = setup dir in
    deposit alice "10.00";
    (* The monthly fee was charged twice. *)
    List.iter [ "fee-a"; "fee-b" ] ~f:(fun k ->
      ignore
        (Journal.transfer
           j
           ~idempotency_key:(key k)
           ~code:Fee
           ~effective_at:(Journal.now j)
           [ Posting.debit alice (eur "2.50"); Posting.credit fees (eur "2.50") ]
         |> ok_exn
         : Journal.Append_result.t));
    let correction k =
      entry
        k
        ~at:(Journal.now j)
        [ Posting.debit fees (eur "2.50"); Posting.credit alice (eur "2.50") ]
      |> ok_exn
    in
    (* Booking the Adjustment code directly is refused. *)
    let direct =
      Entry.create
        ~idempotency_key:(key "direct")
        ~kind:(Transfer "adj")
        ~code:
          (Adjustment { maker = "op-1"; checker = "op-2"; reason = "r"; ticket = "T-1" })
        ~effective_at:(Journal.now j)
        ~recorded_at:(Journal.now j)
        [ Posting.debit fees (eur "1.00"); Posting.credit alice (eur "1.00") ]
      |> ok_exn
    in
    print_error ~dir (Journal.append_entry j direct);
    show
      (Journal.propose_adjustment
         j
         (correction "adj-1")
         ~maker:"op-1"
         ~reason:"fee charged twice"
         ~ticket:"OPS-17");
    bal j alice;
    print_error
      ~dir
      (Journal.approve_adjustment j ~idempotency_key:(key "adj-1") ~checker:"op-1");
    show (Journal.approve_adjustment j ~idempotency_key:(key "adj-1") ~checker:"op-2");
    bal j alice;
    print_s
      [%sexp
        (Balances.find_entry (Journal.balances j) (key "adj-1")
         |> Option.map ~f:(fun (_, e) -> e.code)
         : Entry.Code.t option)];
    print_error
      ~dir
      (Journal.approve_adjustment j ~idempotency_key:(key "adj-1") ~checker:"op-3");
    (* A proposal left for more than a day can no longer be approved; rejecting it frees
       nothing and its key stays used. *)
    show
      (Journal.propose_adjustment
         j
         (correction "adj-2")
         ~maker:"op-1"
         ~reason:"r"
         ~ticket:"OPS-18");
    Time_source.advance_directly
      clock
      ~to_:(Time_ns.add (Journal.now j) (Time_ns.Span.of_hr 25.));
    print_error
      ~dir
      (Journal.approve_adjustment j ~idempotency_key:(key "adj-2") ~checker:"op-2");
    show (Journal.reject_adjustment j ~idempotency_key:(key "adj-2") ~checker:"op-2");
    print_s
      [%sexp
        (Balances.find_rejection (Journal.balances j) (key "adj-2")
         : (int * string) option)]);
  [%expect
    {|
    ("adjustments are booked only through proposal and approval"
     (idempotency_key direct))
    (Ok 14)
    (customer:alice (available "5.00 EUR") (held "0.00 EUR") (posted "5.00 EUR"))
    ("an adjustment's maker and checker must differ" op-1 op-1)
    (Ok 15)
    (customer:alice (available "7.50 EUR") (held "0.00 EUR") (posted "7.50 EUR"))
    ((Adjustment (maker op-1) (checker op-2) (reason "fee charged twice")
      (ticket OPS-17)))
    ("proposal already decided" (idempotency_key adj-1) (status Approved))
    (Ok 16)
    ("proposal expired" (idempotency_key adj-2))
    (Ok 17)
    ((17 "adjustment rejected by op-2"))
    |}]
;;

let%expect_test "day close writes a snapshot that replay reproduces" =
  with_dir (fun dir ->
    let clock, j, deposit = setup dir in
    deposit alice "100.00";
    ignore
      (Journal.hold
         j
         ~hold_id:(Hold_id.of_string "a1")
         ~account:alice
         ~amount:(eur "30.00")
         ~idempotency_key:(key "h1")
         ~effective_at:(Journal.now j)
         ()
       |> ok_exn
       : Journal.Append_result.t);
    let s1 = Journal.close_day j |> ok_exn in
    print_s
      [%sexp
        (s1.rows |> List.filter ~f:(fun r -> Account_id.equal r.account alice)
         : Balances.Day_row.t list)];
    Time_source.advance_directly clock ~to_:(at (60 * 24));
    deposit alice "5.00" ~k:"dep-2";
    let s2 = Journal.close_day j |> ok_exn in
    print_s
      [%sexp
        (s2.rows |> List.filter ~f:(fun r -> Account_id.equal r.account alice)
         : Balances.Day_row.t list)];
    deposit alice "1.00" ~k:"dep-3";
    let live = Journal.balances j in
    Journal.close j;
    let path = dir ^/ "j.bin" in
    let s2' =
      Snapshot.read (Snapshot.default_path ~journal:path s2.business_date) |> ok_exn
    in
    print_error (Snapshot.verify s2' ~journal:path);
    let from_snapshot = Journal.open_ ~fsync:false ~snapshot:s2' path |> ok_exn in
    print_s
      [%message
        ""
          ~same_as_live:(Balances.equal (Journal.balances from_snapshot) live : bool)
          ~records_replayed:(List.length (Journal.records from_snapshot) : int)];
    Journal.close from_snapshot;
    (* Rewrite one byte inside the covered part of the journal: the snapshot no longer
       matches, and the chain breaks for a full replay too. *)
    let contents = Bytes.of_string (In_channel.read_all path) in
    Bytes.set contents 200 (Char.of_int_exn (Char.to_int (Bytes.get contents 200) lxor 1));
    Out_channel.write_all path ~data:(Bytes.to_string contents);
    print_error ~dir (Snapshot.verify s2' ~journal:path);
    print_error ~dir (Journal.open_ ~snapshot:s2' path);
    print_error ~dir (Journal.open_ path));
  [%expect
    {|
    (((account customer:alice) (currency EUR) (opening_booked "0.00 EUR")
      (debits "0.00 EUR") (credits "100.00 EUR") (closing_booked "100.00 EUR")
      (closing_available "70.00 EUR")))
    (((account customer:alice) (currency EUR) (opening_booked "100.00 EUR")
      (debits "0.00 EUR") (credits "5.00 EUR") (closing_booked "105.00 EUR")
      (closing_available "75.00 EUR")))
    ok
    ((same_as_live true) (records_replayed 1))
    ((Snapshot.verify $DIR/j.bin)
     ("journal prefix does not match the snapshot"
      (expected f5c3df9c2f86ca862db732c31098458e)
      (found 32723ad0f91e11ef55c27fc4ebb5cc5a)))
    ((Journal.open_ $DIR/j.bin) "snapshot does not match this journal")
    ((Journal.open_ $DIR/j.bin)
     ("corrupt journal"
      (tail (Corrupt (offset 128) (reason "checksum mismatch")))))
    |}]
;;

let%expect_test "camt.053 statement" =
  with_dir (fun dir ->
    let clock, j, deposit = setup dir in
    deposit alice "1500.00";
    let ok r = ignore (ok_exn r : Journal.Append_result.t) in
    ok
      (Journal.transfer
         j
         ~idempotency_key:(key "rent")
         ~code:Withdrawal
         ~reference:"e2e-rent-10"
         ~remittance_info:"rent october"
         ~effective_at:(Journal.now j)
         [ Posting.debit alice (eur "850.00"); Posting.credit network (eur "850.00") ]);
    ok
      (Journal.hold
         j
         ~hold_id:(Hold_id.of_string "auth-7")
         ~account:alice
         ~amount:(eur "42.10")
         ~code:Card_auth
         ~reference:"card-coffee"
         ~idempotency_key:(key "auth-7")
         ~effective_at:(Journal.now j)
         ());
    ignore (Journal.close_day j |> ok_exn : Snapshot.t);
    Time_source.advance_directly clock ~to_:(at (60 * 24));
    ok
      (Journal.capture
         j
         ~hold_id:(Hold_id.of_string "auth-7")
         ~amount:(eur "39.90")
         ~code:Card_settlement
         ~reference:"card-coffee"
         ~to_:card_scheme
         ~idempotency_key:(key "clear-7")
         ~effective_at:(at 30)
         ());
    ok
      (Journal.hold
         j
         ~hold_id:(Hold_id.of_string "auth-8")
         ~account:alice
         ~amount:(eur "12.00")
         ~code:Card_auth
         ~reference:"card-books"
         ~idempotency_key:(key "auth-8")
         ~effective_at:(Journal.now j)
         ());
    ok
      (Journal.reverse
         j
         ~reverses:(key "rent")
         ~idempotency_key:(key "rent-back")
         ~effective_at:(Journal.now j)
         ());
    let statement ~from ~to_ =
      Camt053.create
        ~include_pending:true
        (Journal.records j)
        ~account:alice
        ~currency:eur_
        ~from_date:(Date.of_string from)
        ~to_date:(Date.of_string to_)
        ~created_at:(Journal.now j)
      |> ok_exn
    in
    print_string (Camt053.to_string_hum (statement ~from:"2026-10-01" ~to_:"2026-10-01"));
    print_string (Camt053.to_string_hum (statement ~from:"2026-10-02" ~to_:"2026-10-02")));
  [%expect
    {|
    camt.053 customer:alice/EUR/2026-10-01 customer:alice EUR 2026-10-01..2026-10-01
      OPBD 2026-10-01 CRDT 0.00 EUR
      CLBD 2026-10-01 CRDT 650.00 EUR
      CLAV 2026-10-01 CRDT 607.90 EUR
      10.0     BOOK 2026-10-01 2026-10-01 CRDT      1500.00 EUR DEPOSIT           e2e-dep-1    salary
      11.0     BOOK 2026-10-01 2026-10-01 DBIT       850.00 EUR WITHDRAWAL        e2e-rent-10  rent october
      12.hold  PDNG -          2026-10-01 DBIT        42.10 EUR CARD_AUTH         card-coffee
    camt.053 customer:alice/EUR/2026-10-02 customer:alice EUR 2026-10-02..2026-10-02
      OPBD 2026-10-02 CRDT 650.00 EUR
      CLBD 2026-10-02 CRDT 1460.10 EUR
      CLAV 2026-10-02 CRDT 1445.90 EUR
      14.0     BOOK 2026-10-02 2026-10-01 DBIT        39.90 EUR CARD_SETTLEMENT   card-coffee
      16.0     BOOK 2026-10-02 2026-10-02 CRDT       850.00 EUR REVERSAL          e2e-rent-10   (reversal)
      12.hold  PDNG -          2026-10-01 DBIT         2.20 EUR CARD_AUTH         card-coffee
      15.hold  PDNG -          2026-10-02 DBIT        12.00 EUR CARD_AUTH         card-books
    |}]
;;

let%expect_test "clearing accounts that do not return to zero" =
  with_dir (fun dir ->
    let clock, j, deposit = setup dir in
    deposit alice "100.00";
    let ok r = ignore (ok_exn r : Journal.Append_result.t) in
    let step k postings =
      ok
        (Journal.transfer
           j
           ~idempotency_key:(key k)
           ~effective_at:(Journal.now j)
           postings)
    in
    (* Two SEPA payments go out through clearing; only the first is confirmed. *)
    step "p1" [ Posting.debit alice (eur "10.00"); Posting.credit sepa_out (eur "10.00") ];
    step "p2" [ Posting.debit alice (eur "20.00"); Posting.credit sepa_out (eur "20.00") ];
    step
      "p1-ok"
      [ Posting.debit sepa_out (eur "10.00"); Posting.credit network (eur "10.00") ];
    Time_source.advance_directly clock ~to_:(at (60 * 24 * 3));
    step "fx" [ Posting.debit alice (eur "5.00"); Posting.credit fx (eur "5.00") ];
    let report older_than_days =
      Clearing.open_items
        ~older_than_days
        (Journal.records j)
        ~as_of:(Journal.business_date j)
      |> ok_exn
    in
    print_s [%sexp (report 0 : Clearing.Item.t list)];
    print_s [%sexp (report 1 : Clearing.Item.t list)]);
  [%expect
    {|
    (((account clearing:sepa_out) (currency EUR) (balance "-20.00 EUR")
      (non_zero_since 2026-10-01) (age_days 3))
     ((account clearing:fx) (currency EUR) (balance "-5.00 EUR")
      (non_zero_since 2026-10-04) (age_days 0)))
    (((account clearing:sepa_out) (currency EUR) (balance "-20.00 EUR")
      (non_zero_since 2026-10-01) (age_days 3)))
    |}]
;;

let%expect_test "safeguarding reconciliation" =
  with_dir (fun dir ->
    let _, j, deposit = setup dir in
    deposit alice "100.00";
    deposit bob "50.00" ~k:"dep-b";
    (* A customer payment left safeguarding before the outgoing transfer was booked
       against it: the ledger's safeguarding asset is short. *)
    ignore
      (Journal.transfer
         j
         ~idempotency_key:(key "sweep")
         ~code:Safeguarding_sweep
         ~effective_at:(Journal.now j)
         [ Posting.debit network (eur "30.00")
         ; Posting.credit safeguarding (eur "30.00")
         ]
       |> ok_exn
       : Journal.Append_result.t);
    let b = Journal.balances j in
    print_s
      [%sexp
        (Safeguarding.reconcile
           b
           ~currency:eur_
           ~business_date:(Journal.business_date j)
           ~external_balance:(eur "125.00")
         : Safeguarding.Recon.t Or_error.t)];
    print_s [%sexp (Safeguarding.exposures b : Safeguarding.Exposure.t list)]);
  [%expect
    {|
    (Ok
     ((currency EUR) (business_date 2026-10-01) (requirement "150.00 EUR")
      (internal_resource "120.00 EUR") (external_balance "125.00 EUR")
      (resource_minus_requirement "-30.00 EUR")
      (external_minus_internal "5.00 EUR") (status Shortfall)
      (proposed_top_up "30.00 EUR")))
    (((currency EUR) (customer_liability "150.00 EUR") (assets_held "120.00 EUR")
      (house_position "-30.00 EUR")))
    |}]
;;

let%expect_test "hold entries carry SCA evidence" =
  with_dir (fun dir ->
    let _, j, deposit = setup dir in
    deposit alice "100.00";
    let h = Hold_id.of_string "auth-sca" in
    let ok r = ignore (ok_exn r : Journal.Append_result.t) in
    let now () = Journal.now j in
    ok
      (Journal.hold
         j
         ~hold_id:h
         ~account:alice
         ~amount:(eur "20.00")
         ~sca:(Challenge "sca-chl-1")
         ~idempotency_key:(key "auth")
         ~effective_at:(now ())
         ());
    ok
      (Journal.increment
         j
         ~hold_id:h
         ~amount:(eur "5.00")
         ~sca:(Exemption "low_value")
         ~idempotency_key:(key "inc")
         ~effective_at:(now ())
         ());
    ok
      (Journal.capture
         j
         ~hold_id:h
         ~amount:(eur "10.00")
         ~sca:(Challenge "sca-chl-1")
         ~to_:card_scheme
         ~idempotency_key:(key "clear-1")
         ~effective_at:(now ())
         ());
    ok
      (Journal.over_capture
         j
         ~hold_id:h
         ~amount:(eur "16.00")
         ~reason:"tip"
         ~sca:(Exemption "merchant_initiated")
         ~to_:card_scheme
         ~idempotency_key:(key "clear-2")
         ~effective_at:(now ())
         ());
    let b = Journal.balances j in
    List.iter [ "auth"; "inc"; "clear-1"; "clear-2" ] ~f:(fun k ->
      let _, (e : Entry.t) = Balances.find_entry b (key k) |> Option.value_exn in
      print_s [%message k ~sca:(e.sca : Sca_evidence.t option)]);
    (* The evidence survives the journal file. *)
    Journal.close j;
    let replayed = Journal.replay (dir ^/ "j.bin") |> ok_exn in
    let _, e = Balances.find_entry replayed (key "auth") |> Option.value_exn in
    print_s [%sexp (e.sca : Sca_evidence.t option)]);
  [%expect
    {|
    (auth (sca ((Challenge sca-chl-1))))
    (inc (sca ((Exemption low_value))))
    (clear-1 (sca ((Challenge sca-chl-1))))
    (clear-2 (sca ((Exemption merchant_initiated))))
    ((Challenge sca-chl-1))
    |}]
;;

let%expect_test "linked groups on a frozen clock get increasing ledger timestamps" =
  with_dir (fun dir ->
    (* The clock never moves: every timestamp comes from the ledger's own increasing
       assignment, whether the record is a group, a refusal or a single entry. *)
    let _, j, deposit = setup dir in
    deposit alice "100.00";
    let group prefix ~amount =
      List.map [ "a"; "b"; "c" ] ~f:(fun leg ->
        entry
          (prefix ^ "-" ^ leg)
          ~at:(at 0)
          [ Posting.debit alice (eur amount); Posting.credit bob (eur amount) ]
        |> ok_exn)
    in
    let show_result r =
      print_s [%sexp (Or_error.map r ~f:Journal.Append_result.seq : int Or_error.t)]
    in
    show_result (Journal.append_linked j (group "g1" ~amount:"1.00"));
    (* Too much for Alice: refused as a whole, its keys remembered. *)
    show_result
      (Journal.append_linked j (group "g2" ~amount:"50.00")
       |> Result.map_error ~f:(fun _ -> Error.of_string "refused"));
    show_result (Journal.append_linked j (group "g3" ~amount:"1.00"));
    show_result (Journal.append_linked j (group "g4" ~amount:"1.00"));
    deposit alice "1.00" ~k:"dep-2";
    let stamps =
      Journal.records j
      |> List.filter_map ~f:(fun (r : Record.t) ->
        match r.event with
        | Posted e -> Some [ r.seq, e.recorded_at ]
        | Linked es -> Some (List.map es ~f:(fun e -> r.seq, e.recorded_at))
        | Rejected { recorded_at; _ } -> Some [ r.seq, recorded_at ]
        | _ -> None)
      |> List.concat
    in
    List.iter stamps ~f:(fun (seq, at) ->
      print_endline
        (sprintf
           "%3d %s"
           seq
           (Time_ns.to_string_iso8601_basic at ~zone:Time_float.Zone.utc)));
    let times = List.map stamps ~f:snd in
    assert (List.is_sorted_strictly times ~compare:Time_ns.compare);
    Journal.close j;
    ignore (Journal.replay (dir ^/ "j.bin") |> ok_exn : Balances.t));
  [%expect
    {|
    (Ok 11)
    (Error refused)
    (Ok 15)
    (Ok 16)
     10 2026-10-01T09:00:00.000000000Z
     11 2026-10-01T09:00:00.000000001Z
     11 2026-10-01T09:00:00.000000002Z
     11 2026-10-01T09:00:00.000000003Z
     12 2026-10-01T09:00:00.000000004Z
     13 2026-10-01T09:00:00.000000005Z
     14 2026-10-01T09:00:00.000000006Z
     15 2026-10-01T09:00:00.000000007Z
     15 2026-10-01T09:00:00.000000008Z
     15 2026-10-01T09:00:00.000000009Z
     16 2026-10-01T09:00:00.000000010Z
     16 2026-10-01T09:00:00.000000011Z
     16 2026-10-01T09:00:00.000000012Z
     17 2026-10-01T09:00:00.000000013Z
    |}]
;;
