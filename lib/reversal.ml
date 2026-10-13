open! Core

let create balances ~seq ~idempotency_key ?reference ~effective_at ~recorded_at () =
  match Balances.find_entry balances ~seq with
  | None -> Or_error.error_s [%message "no entry with this sequence number" (seq : int)]
  | Some original ->
    Entry.create
      ~idempotency_key
      ~kind:(Reversal { reverses = seq })
      ~reference:(Option.value reference ~default:original.reference)
      ~effective_at
      ~recorded_at
      (List.map original.postings ~f:Posting.flip)
;;
