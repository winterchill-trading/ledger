open! Core
module Amount = Money.Amount

let account_exn balances id = Option.value_exn (Balances.find_account balances id)

let out_side balances id =
  Side.flip (Account_kind.normal_side (account_exn balances id).kind)
;;

let place
  balances
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
  =
  match Balances.find_account balances account with
  | None ->
    Or_error.error_s
      [%message "hold on an account that is not open" (account : Account_id.t)]
  | Some _ ->
    let out = out_side balances account in
    Entry.create
      ~idempotency_key
      ~kind:
        (match expires_at with
         | None -> Hold_place hold_id
         | Some expires_at -> Hold_place_expiring { hold = hold_id; expires_at })
      ?code
      ?reference
      ?remittance_info
      ~effective_at
      ~recorded_at
      [ { account; side = out; amount }
      ; { account = Account_id.pending account; side = Side.flip out; amount }
      ]
;;

let open_hold balances hold_id =
  match Balances.find_hold balances hold_id with
  | None -> Or_error.error_s [%message "unknown hold" (hold_id : Hold_id.t)]
  | Some h when Amount.is_zero h.remaining ->
    Or_error.error_s [%message "hold is already closed" (hold_id : Hold_id.t)]
  | Some h -> Ok h
;;

let return_entry
  ~kind
  balances
  ~hold_id
  ?amount
  ~idempotency_key
  ?code
  ?reference
  ~effective_at
  ~recorded_at
  ()
  =
  let%bind.Or_error h = open_hold balances hold_id in
  let amount = Option.value amount ~default:h.remaining in
  let out = out_side balances h.account in
  Entry.create
    ~idempotency_key
    ~kind
    ?code
    ?reference
    ~effective_at
    ~recorded_at
    [ { account = Account_id.pending h.account; side = out; amount }
    ; { account = h.account; side = Side.flip out; amount }
    ]
;;

let increment
  balances
  ~hold_id
  ~amount
  ~idempotency_key
  ?code
  ?reference
  ~effective_at
  ~recorded_at
  ()
  =
  let%bind.Or_error h = open_hold balances hold_id in
  let out = out_side balances h.account in
  Entry.create
    ~idempotency_key
    ~kind:(Hold_increment hold_id)
    ?code
    ?reference
    ~effective_at
    ~recorded_at
    [ { account = h.account; side = out; amount }
    ; { account = Account_id.pending h.account; side = Side.flip out; amount }
    ]
;;

let release balances ~hold_id =
  return_entry ~kind:(Hold_release hold_id) balances ~hold_id
;;

let expire balances ~hold_id ~idempotency_key ~effective_at ~recorded_at () =
  return_entry
    ~kind:(Hold_expire hold_id)
    balances
    ~hold_id
    ~idempotency_key
    ~effective_at
    ~recorded_at
    ()
;;

let capture_split
  balances
  ~hold_id
  ?(release_remainder = false)
  payees
  ~idempotency_key
  ?code
  ?reference
  ?remittance_info
  ~effective_at
  ~recorded_at
  ()
  =
  let open Or_error.Let_syntax in
  let%bind h = open_hold balances hold_id in
  let currency = Amount.currency h.remaining in
  let%bind paid = Amount.sum currency (List.map payees ~f:snd) in
  let out = out_side balances h.account in
  let pending = Account_id.pending h.account in
  let rest = Amount.sub_exn h.remaining paid in
  let from_pending, back =
    if release_remainder && Amount.is_positive rest
    then
      ( h.remaining
      , [ { Posting.account = h.account; side = Side.flip out; amount = rest } ] )
    else paid, []
  in
  Entry.create
    ~idempotency_key
    ~kind:(Hold_capture hold_id)
    ?code
    ?reference
    ?remittance_info
    ~effective_at
    ~recorded_at
    (({ Posting.account = pending; side = out; amount = from_pending } :: back)
     @ List.map payees ~f:(fun (account, amount) ->
       { Posting.account; side = Side.flip out; amount }))
;;

let capture
  balances
  ~hold_id
  ?amount
  ?release_remainder
  ~to_
  ~idempotency_key
  ?code
  ?reference
  ?remittance_info
  ~effective_at
  ~recorded_at
  ()
  =
  let%bind.Or_error h = open_hold balances hold_id in
  capture_split
    balances
    ~hold_id
    ?release_remainder
    [ to_, Option.value amount ~default:h.remaining ]
    ~idempotency_key
    ?code
    ?reference
    ?remittance_info
    ~effective_at
    ~recorded_at
    ()
;;

let over_capture
  balances
  ~hold_id
  ~amount
  ~reason
  ~to_
  ~idempotency_key
  ?code
  ?reference
  ?remittance_info
  ~effective_at
  ~recorded_at
  ()
  =
  let open Or_error.Let_syntax in
  let%bind h = open_hold balances hold_id in
  let%bind excess = Amount.sub amount h.remaining in
  if not (Amount.is_positive excess)
  then
    Or_error.error_s
      [%message
        "an over-capture must exceed the hold; use capture"
          (hold_id : Hold_id.t)
          ~amount:(Amount.to_string amount)
          ~remaining:(Amount.to_string h.remaining)]
  else (
    let out = out_side balances h.account in
    Entry.create
      ~idempotency_key
      ~kind:(Hold_over_capture { hold = hold_id; reason })
      ?code
      ?reference
      ?remittance_info
      ~effective_at
      ~recorded_at
      [ { account = Account_id.pending h.account; side = out; amount = h.remaining }
      ; { account = h.account; side = out; amount = excess }
      ; { account = to_; side = Side.flip out; amount }
      ])
;;
