# Changes

All notable changes to this library. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); versions follow semantic
versioning once released.

## Unreleased

### Added

- `Framing.fold` and `Framing.fold_file`: decode journal frames from a channel one at a
  time, optionally up to a byte offset (`?until`).
- `Journal.fold`: stream a journal file for reports; `?keep_records:false` on
  `Journal.create` and `Journal.open_` keeps only the folded state in memory.
- `Chain_head` and `Journal.chain_head`: the hash-chain head as one line of text to
  publish, and `Chain_head.verify` to check a journal still contains it. `ledger verify`
  prints it and takes `-head`.
- `Journal.expire_holds_at`, `Journal.next_expiry` and `Balances.next_expiry`, so a
  caller can drive hold expiry from its own timer.
- `?refusal_retention` on `Journal.create` and `Journal.open_`,
  `Journal.forget_expired_refusals`, `Balances.refusals` and the `Refusals_forgotten`
  record: refusals last a retention window instead of forever. The new record type has a
  new digest; journals written before it still read.
- `examples/quickstart.ml`, built and run in CI, and an odoc index page.

### Changed

- Opening, replaying and reading a journal stream the file instead of reading it whole.
- Refusals by the hold, transfer and reversal builders (an unknown hold id, a malformed
  entry) are remembered under their idempotency key, like refusals by the ledger rules.
- The CLI's sample journal command is `example` (was `demo`).

### Fixed

- `Journal.open_account` for an account opened before the snapshot a journal was
  reopened from returns `Duplicate` instead of raising.

## 0.1.0 (not released)

First version: accounts with kinds, purposes and statuses; balanced entries with codes,
value dates, ledger-assigned time and business dates; holds with increments, split and
over-captures, release and expiry; reversals; linked groups; four-eyes adjustments;
remembered refusals; day close with snapshots; trial balance, statements, camt.053,
clearing and safeguarding reports; the checksummed, hash-chained journal file and the
`ledger` command.
