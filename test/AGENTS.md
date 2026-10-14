# test: ledger tests

- Expect tests per feature (`test_balances.ml`, `test_journal.ml`, `test_hold*` and others), and
  property tests (`test_properties.ml`) over random valid and invalid operations through a real
  journal, with day closes in between.
- `run.t` is the CLI cram test. Promote its output with `dune promote` only after reading the diff.
- `test_compat.ml` (and `test_readme.ml` for the README examples) reads `data/digest-v1.journal` with today's code. Never regenerate the file;
  a failing compat test means the reader broke, not that the fixture is stale.
