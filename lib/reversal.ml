open! Core

let create
  balances
  ~reverses
  ~idempotency_key
  ?code
  ?reference
  ?remittance_info
  ~effective_at
  ~recorded_at
  ()
  =
  match Balances.find_entry balances reverses with
  | None ->
    Or_error.error_s
      [%message "no entry booked under this key" (reverses : Idempotency_key.t)]
  | Some (_, original) ->
    Entry.create
      ~idempotency_key
      ~kind:(Reversal { reverses })
      ?code
      ~reference:(Option.value reference ~default:original.reference)
      ?remittance_info
      ~effective_at
      ~recorded_at
      (List.map original.postings ~f:Posting.flip)
;;
