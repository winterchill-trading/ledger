# bin: ledger CLI

`main.ml` is the `ledger` executable: `trial-balance`, `statement`, `camt053`, `verify` (journal
and `-head` chain check), and `example`, which writes a sample journal.

Rules:

- The CLI reads a journal file; it never writes to one except through `example`.
- Output is text or statement records; keep the field order stable, since the cram test in `test/`
  pins it.
- `camt053` reads every record into memory, because the statement replays the journal for its
  balances. Say so in any change that touches it.
