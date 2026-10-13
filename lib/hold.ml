open! Core

let account_exn balances id = Option.value_exn (Balances.find_account balances id)

let place
  balances
  ~hold_id
  ~account
  ~amount
  ~idempotency_key
  ?reference
  ~effective_at
  ~recorded_at
  ()
  =
  match Balances.find_account balances account with
  | None ->
    Or_error.error_s
      [%message "hold on an account that is not open" (account : Account_id.t)]
  | Some { kind; _ } ->
    let into_pending = Account_kind.normal_side kind in
    Entry.create
      ~idempotency_key
      ~kind:(Hold_place hold_id)
      ?reference
      ~effective_at
      ~recorded_at
      [ { account; side = Side.flip into_pending; amount }
      ; { account = Account_id.pending account; side = into_pending; amount }
      ]
;;

let open_hold balances hold_id ~amount =
  match Balances.find_hold balances hold_id with
  | None -> Or_error.error_s [%message "unknown hold" (hold_id : Hold_id.t)]
  | Some h when Money.Amount.is_zero h.remaining ->
    Or_error.error_s [%message "hold is already closed" (hold_id : Hold_id.t)]
  | Some h -> Ok (h, Option.value amount ~default:h.remaining)
;;

let release
  balances
  ~hold_id
  ?amount
  ~idempotency_key
  ?reference
  ~effective_at
  ~recorded_at
  ()
  =
  let%bind.Or_error h, amount = open_hold balances hold_id ~amount in
  let back = Account_kind.normal_side (account_exn balances h.account).kind in
  Entry.create
    ~idempotency_key
    ~kind:(Hold_release hold_id)
    ?reference
    ~effective_at
    ~recorded_at
    [ { account = Account_id.pending h.account; side = Side.flip back; amount }
    ; { account = h.account; side = back; amount }
    ]
;;

let capture
  balances
  ~hold_id
  ?amount
  ~to_
  ~idempotency_key
  ?reference
  ~effective_at
  ~recorded_at
  ()
  =
  let%bind.Or_error h, amount = open_hold balances hold_id ~amount in
  let out = Side.flip (Account_kind.normal_side (account_exn balances h.account).kind) in
  Entry.create
    ~idempotency_key
    ~kind:(Hold_capture hold_id)
    ?reference
    ~effective_at
    ~recorded_at
    [ { account = Account_id.pending h.account; side = out; amount }
    ; { account = to_; side = Side.flip out; amount }
    ]
;;
