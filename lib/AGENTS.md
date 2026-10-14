# lib: ledger library

Library `ledger` (module `Ledger`). Modules group into three layers:

- Identifiers and values: `Validated_id`, `Account_id`, `Hold_id`, `Idempotency_key`, `Side`,
  `Account_kind`, `Account`, `Account_status`, `Sca_evidence`, `Posting`, `Entry`.
- Storage: `Record`, `Framing`, `Journal`, `Chain_head`, `Snapshot`. The journal is appended,
  replayed and folded; it is streamed on open, not read whole.
- Behaviour: `Balances`, `Hold`, `Reversal`, `Trial_balance`, `Statement`, `Camt053`, `Clearing`,
  `Safeguarding`.

Rules:

- `Journal.append_linked` commits a group or refuses it as one; a partial group is never written.
- Each record carries the hash of the previous one. A change to what is hashed changes the chain
  head format and needs a version bump.
- Manual adjustments need a second approver; the check lives in the journal, not in the caller.
- Keep `Camt053` free of XML; it returns values.

Rules that hold after any accepted sequence are listed in `../README.md` under "Rules".
