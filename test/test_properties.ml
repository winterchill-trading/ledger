open! Core
open Ledger
open Helpers
module Amount = Money.Amount
module Currency = Money.Currency

(* Random operations against the [Helpers.chart] bank. Many are invalid on purpose
   (overdrafts, captures of closed holds, double reversals): the ledger must refuse them
   and keep its invariants. *)
module Op = struct
  type t =
    | Deposit of
        { customer : int
        ; currency : int
        ; units : int
        }
    | Transfer of
        { from : int
        ; to_ : int
        ; currency : int
        ; units : int
        }
    | Withdraw of
        { customer : int
        ; currency : int
        ; units : int
        }
    | Place of
        { customer : int
        ; currency : int
        ; units : int
        ; hold : int
        }
    | Capture of
        { hold : int
        ; units : int option
        }
    | Release of
        { hold : int
        ; units : int option
        }
    | Reverse of { back : int }
  [@@deriving sexp_of, quickcheck]
end

let customers = [| alice; bob |]
let currencies = [| Currency.eur; Currency.btc |]
let pick a i = a.(Int.abs i % Array.length a)

let amount currency units =
  Amount.of_minor_units_int currency (1 + (Int.abs units % 50_000))
;;

let hold_id i = Hold_id.of_string (sprintf "h%d" (Int.abs i % 6))

let gen_ops =
  let open Quickcheck.Generator.Let_syntax in
  let%bind n = Int.gen_incl 0 60 in
  List.gen_with_length n Op.quickcheck_generator
;;

(* Builds the entry for [op] in state [b], [None] when the op does not even build. *)
let build b ~i (op : Op.t) =
  let k = key (sprintf "op-%d" i) in
  let at = at i in
  let entry postings = entry (sprintf "op-%d" i) ~at postings in
  match op with
  | Deposit { customer; currency; units } ->
    let a = amount (pick currencies currency) units in
    entry [ Posting.debit central_bank a; Posting.credit (pick customers customer) a ]
  | Transfer { from; to_; currency; units } ->
    let a = amount (pick currencies currency) units in
    entry [ Posting.debit (pick customers from) a; Posting.credit (pick customers to_) a ]
  | Withdraw { customer; currency; units } ->
    let a = amount (pick currencies currency) units in
    entry [ Posting.debit (pick customers customer) a; Posting.credit network a ]
  | Place { customer; currency; units; hold } ->
    Hold.place
      b
      ~hold_id:(hold_id hold)
      ~account:(pick customers customer)
      ~amount:(amount (pick currencies currency) units)
      ~idempotency_key:k
      ~effective_at:at
      ~recorded_at:at
      ()
  | Capture { hold; units } ->
    let%bind.Or_error h =
      Balances.find_hold b (hold_id hold)
      |> Or_error.of_option ~error:(Error.of_string "no hold")
    in
    Hold.capture
      b
      ~hold_id:(hold_id hold)
      ?amount:(Option.map units ~f:(amount (Amount.currency h.placed)))
      ~to_:card_scheme
      ~idempotency_key:k
      ~effective_at:at
      ~recorded_at:at
      ()
  | Release { hold; units } ->
    let%bind.Or_error h =
      Balances.find_hold b (hold_id hold)
      |> Or_error.of_option ~error:(Error.of_string "no hold")
    in
    Hold.release
      b
      ~hold_id:(hold_id hold)
      ?amount:(Option.map units ~f:(amount (Amount.currency h.placed)))
      ~idempotency_key:k
      ~effective_at:at
      ~recorded_at:at
      ()
  | Reverse { back } ->
    Reversal.create
      b
      ~seq:(Int.max 1 (Balances.next_seq b - 1 - (Int.abs back % 8)))
      ~idempotency_key:k
      ~effective_at:at
      ~recorded_at:at
      ()
;;

(* Applies every op that builds and is accepted; returns the state and its records. *)
let run ops =
  let opened, records =
    List.fold chart ~init:(Balances.empty, []) ~f:(fun (b, rs) a ->
      let r = { Record.seq = Balances.next_seq b; event = Account_opened a } in
      Balances.apply b r |> ok_exn, r :: rs)
  in
  List.foldi ops ~init:(opened, records, []) ~f:(fun i (b, rs, states) op ->
    match build b ~i op with
    | Error _ -> b, rs, states
    | Ok entry ->
      let r = { Record.seq = Balances.next_seq b; event = Posted entry } in
      (match Balances.apply b r with
       | Error _ -> b, rs, states
       | Ok b -> b, r :: rs, b :: states))
  |> fun (b, rs, states) -> b, List.rev rs, states
;;

let check_invariants b =
  Trial_balance.of_balances b |> Trial_balance.check |> ok_exn;
  List.iter (Balances.accounts b) ~f:(fun account ->
    Array.iter currencies ~f:(fun currency ->
      if not account.allow_negative
      then
        if Amount.is_negative (Balances.available b account.id currency)
        then raise_s [%message "negative available" (account.id : Account_id.t)];
      (* Held funds equal the open holds' remaining amounts. *)
      let open_holds =
        List.filter (Balances.open_holds b) ~f:(fun h ->
          Account_id.equal h.account account.id
          && Currency.equal (Amount.currency h.remaining) currency)
        |> List.map ~f:(fun h -> h.remaining)
        |> Amount.sum currency
        |> ok_exn
      in
      [%test_result: Amount.t] (Balances.held b account.id currency) ~expect:open_holds))
;;

let%test_unit "every accepted sequence keeps the books balanced and nobody overdrawn" =
  Quickcheck.test gen_ops ~trials:300 ~sexp_of:[%sexp_of: Op.t list] ~f:(fun ops ->
    let _, _, states = run ops in
    List.iter states ~f:check_invariants)
;;

let summary b = Trial_balance.of_balances b, Balances.open_holds b

let%test_unit "replaying the records reproduces the balances" =
  Quickcheck.test gen_ops ~trials:300 ~sexp_of:[%sexp_of: Op.t list] ~f:(fun ops ->
    let b, records, _ = run ops in
    let replayed = Balances.of_records records |> ok_exn in
    [%test_result: Trial_balance.t * Balances.Hold_state.t list]
      (summary replayed)
      ~expect:(summary b))
;;

let%test_unit "a journal file replays to the same balances" =
  let dir = Filename_unix.temp_dir "ledger" "prop" in
  Quickcheck.test gen_ops ~trials:40 ~sexp_of:[%sexp_of: Op.t list] ~f:(fun ops ->
    let b, records, _ = run ops in
    let path = Filename_unix.temp_file ~in_dir:dir "j" ".bin" in
    Core_unix.unlink path;
    let j = Journal.create ~fsync:false path |> ok_exn in
    List.iter records ~f:(fun (r : Record.t) ->
      match r.event with
      | Account_opened a ->
        ignore (Journal.open_account j a |> ok_exn : Journal.Append_result.t)
      | Posted e -> ignore (Journal.append_entry j e |> ok_exn : Journal.Append_result.t));
    Journal.close j;
    [%test_result: Record.t list] (Journal.read path |> ok_exn |> fst) ~expect:records;
    [%test_result: Trial_balance.t * Balances.Hold_state.t list]
      (summary (Journal.replay path |> ok_exn))
      ~expect:(summary b);
    Core_unix.unlink path);
  Core_unix.rmdir dir
;;

let%expect_test "the generator exercises every kind of entry" =
  (* Guards against a generator that only ever produces rejected operations. *)
  let counts = String.Table.create () in
  Quickcheck.iter gen_ops ~trials:200 ~f:(fun ops ->
    let _, records, _ = run ops in
    List.iter records ~f:(fun (r : Record.t) ->
      match r.event with
      | Account_opened _ -> ()
      | Posted e ->
        let name =
          match e.kind with
          | Transfer _ -> "transfer"
          | Hold_place _ -> "hold"
          | Hold_capture _ -> "capture"
          | Hold_release _ -> "release"
          | Reversal _ -> "reversal"
        in
        Hashtbl.incr counts name));
  print_s [%sexp (Hashtbl.keys counts |> List.sort ~compare:String.compare : string list)];
  [%expect {| (capture hold release reversal transfer) |}]
;;
