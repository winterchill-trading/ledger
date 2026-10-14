open! Core
open Ledger
open Helpers
open Test_journal
module Time_source = Async_kernel.Time_source

let hour = Time_ns.Span.of_hr 1.

(* Alice has nothing, so a withdrawal is refused; after a deposit it would succeed. *)
let withdraw j k =
  Journal.transfer
    j
    ~idempotency_key:(key k)
    ~effective_at:(at 0)
    [ Posting.debit alice (eur "10.00"); Posting.credit network (eur "10.00") ]
;;

let deposit j k =
  ignore
    (Journal.transfer
       j
       ~idempotency_key:(key k)
       ~effective_at:(at 0)
       [ Posting.debit central_bank (eur "50.00"); Posting.credit alice (eur "50.00") ]
     |> ok_exn
     : Journal.Append_result.t)
;;

let open_chart j =
  List.iter chart ~f:(fun a ->
    ignore (Journal.open_account j a |> ok_exn : Journal.Append_result.t))
;;

let advance clock span =
  Time_source.advance_directly clock ~to_:(Time_ns.add (Time_source.now clock) span)
;;

let short r =
  match (r : Journal.Append_result.t Or_error.t) with
  | Ok r -> print_s [%message "ok" ~seq:(Journal.Append_result.seq r : int)]
  | Error e ->
    (* The first atom says which rule refused it. *)
    (match [%sexp (e : Error.t)] with
     | List (Atom what :: _) | Atom what -> print_endline what
     | List _ -> print_s [%sexp (e : Error.t)])
;;

let%expect_test "a refusal lasts the retention window, then the key is new again" =
  with_dir (fun dir ->
    let path = dir ^/ "j.bin" in
    let clock = clock () in
    let j =
      Journal.create ~time_source:clock ~fsync:false ~refusal_retention:hour path
      |> ok_exn
    in
    open_chart j;
    short (withdraw j "wd-1");
    deposit j "dep-1";
    advance clock (Time_ns.Span.of_min 59.);
    short (withdraw j "wd-1");
    advance clock (Time_ns.Span.of_min 1.);
    short (withdraw j "wd-1");
    let last_two = List.drop (Journal.records j) (List.length (Journal.records j) - 2) in
    List.iter last_two ~f:(fun r ->
      let event =
        match Record.Event.sexp_of_t r.event with
        | List (Atom name :: _) | Atom name -> name
        | List _ -> "?"
      in
      print_s [%message "" ~seq:(r.seq : int) event]);
    let live = Journal.balances j in
    Journal.close j;
    let reopened = Journal.open_ ~fsync:false ~refusal_retention:hour path |> ok_exn in
    print_s
      [%message
        ""
          ~replay_same:(Balances.equal (Journal.replay path |> ok_exn) live : bool)
          ~reopen_same:(Balances.equal (Journal.balances reopened) live : bool)];
    Journal.close reopened;
    [%expect
      {|
      entry would take an account below zero
      idempotency key already refused
      (ok (seq 10))
      ((seq 9) Refusals_forgotten)
      ((seq 10) Posted)
      ((replay_same true) (reopen_same true))
      |}])
;;

let%expect_test "without a window a refusal stays" =
  with_dir (fun dir ->
    let clock = clock () in
    let j = Journal.create ~time_source:clock ~fsync:false (dir ^/ "j.bin") |> ok_exn in
    open_chart j;
    short (withdraw j "wd-1");
    deposit j "dep-1";
    advance clock (Time_ns.Span.of_day 400.);
    short (withdraw j "wd-1");
    print_s
      [%sexp
        (Journal.forget_expired_refusals j : Journal.Append_result.t option Or_error.t)];
    Journal.close j;
    [%expect
      {|
      entry would take an account below zero
      idempotency key already refused
      (Ok ())
      |}])
;;

let%expect_test "close_day drops every refusal past the window" =
  with_dir (fun dir ->
    let path = dir ^/ "j.bin" in
    let clock = clock () in
    let j =
      Journal.create ~time_source:clock ~fsync:false ~refusal_retention:hour path
      |> ok_exn
    in
    open_chart j;
    short (withdraw j "wd-1");
    advance clock (Time_ns.Span.of_min 30.);
    short (withdraw j "wd-2");
    advance clock (Time_ns.Span.of_min 45.);
    let show j =
      print_s
        [%message
          ""
            ~remembered:
              (List.map (Balances.refusals (Journal.balances j)) ~f:fst
               : Idempotency_key.t list)]
    in
    show j;
    let snapshot = Journal.close_day j |> ok_exn in
    show j;
    Journal.close j;
    (* Reopened from the snapshot, wd-2's refusal counts from the close (75 minutes in, 45
       minutes after the refusal), so it is still remembered 59 minutes after the close
       and dropped soon after 60. *)
    let j =
      Journal.open_ ~time_source:clock ~fsync:false ~refusal_retention:hour ~snapshot path
      |> ok_exn
    in
    advance clock (Time_ns.Span.of_min 59.);
    print_s [%sexp (Journal.forget_expired_refusals j |> ok_exn |> Option.is_some : bool)];
    advance clock (Time_ns.Span.of_min 2.);
    print_s [%sexp (Journal.forget_expired_refusals j |> ok_exn |> Option.is_some : bool)];
    show j;
    Journal.close j;
    [%expect
      {|
      entry would take an account below zero
      entry would take an account below zero
      (remembered (wd-1 wd-2))
      (remembered (wd-2))
      false
      true
      (remembered ())
      |}])
;;

let%expect_test "the window must be positive" =
  with_dir (fun dir ->
    print_error
      (Journal.create ~refusal_retention:Time_ns.Span.zero (dir ^/ "j.bin")
       |> Or_error.map ~f:Journal.close);
    [%expect {| ("refusal_retention must be positive" (span 0s)) |}])
;;
