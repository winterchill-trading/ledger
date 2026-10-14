# ledger: agent guide

## Purpose

An OCaml double-entry ledger: balanced journal entries, two-phase holds, reversals, four-eyes
adjustments, a trial balance and paged account statements, stored in an append-only, checksummed
and hash-chained Bin_prot journal. It knows accounts, postings and idempotency keys, and nothing
of IBANs, cards or webhooks.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | Library `ledger` (module `Ledger`): accounts, postings, holds, the journal, statements, snapshots |
| `bin/` | The `ledger` CLI: `trial-balance`, `statement`, `camt053`, `verify`, `example` |
| `doc/` | `index.mld` and the odoc documentation package |
| `examples/` | `quickstart.ml`, run by CI |
| `test/` | Expect tests, Base_quickcheck properties, the CLI cram test, `test/data/digest-v1.journal` |
| `.github/` | CI workflow |

## Build, test, docs

Run from this repository, inside the workspace:

    opam exec --switch 5.2.0+ox -- dune build --root .. @ledger/all
    opam exec --switch 5.2.0+ox -- dune build --root .. @ledger/runtest
    opam exec --switch 5.2.0+ox -- dune build --root .. @ledger/doc

The docs build must produce no warnings; CI fails on any. Several agents building at once need
`DUNE_BUILD_DIR=_build-ledger`.

Standalone, outside the workspace: add the organization's opam repository
(`opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`), install
the published dependencies, then run `dune build` here.

## Conventions

- Format this repository only: `dune build --root .. @ledger/fmt --auto-promote`. Never
  `dune fmt` at the workspace root.
- Every module has an `.mli`; every public value has an odoc comment, and the docs build is clean.
- Persisted types are `Stable.V1` modules with pinned digests. The journal header digest is
  strict: any change to a persisted type makes old journals unreadable, so it needs a new version
  and a reader for the old one.
- Time is assigned by the ledger, monotonically. Do not take timestamps from callers.
- Records and refusals are kept forever for idempotency unless a retention window is set; a change
  to that rule needs a test on replay.
- User-visible changes get a note under `## Unreleased` in `CHANGES.md`.

## Dependencies

- Depends on sibling repository `money` (which itself depends on `fixed-decimal`).
- External: `core`, `core_unix`, `async_kernel`, `cryptokit`, `ppx_jane`.
- CI pin order: `fixed-decimal`, then `money`, then `ledger`, as in `.github/workflows/ci.yml`.

## CI

`.github/workflows/ci.yml` runs the `infra` ox-setup action with the pins above, then
`dune build`, `dune test`, runs `examples/quickstart.exe`, and builds the odoc docs with warnings
treated as errors. The `obj-magic` job calls the reusable workflow in `winterchill-trading/infra`.
To check a run, use `gh run list --limit 1` and at most one `gh run watch <id> --exit-status`.
Never poll in a loop.

## Pitfalls

- A length-prefixed log cannot tell a damaged length header from a crash-truncated tail. The journal
  stores the length twice (the second copy complemented); keep that, or a repair deletes good frames.
- `lockf` locks belong to the process. Test single-writer locking from a forked child.
- `Time_source` is `Async_kernel.Time_source`, not in Core.
- `Bin_prot.Utils.bin_dump` gives the payload without a size header; `Binable.to_string_with_writer`
  does not exist.
- A first-class module over `Fixed_decimal.S` needs an `immediate` layout annotation, or the build
  fails with "layout of the first is value".
- The compatibility test in `test_compat.ml` shares the test data file; never regenerate
  `test/data/digest-v1.journal`, it is the compatibility fixture.

## Where to read more

- `README.md` (model, rules, journal file, reports, known gaps)
- `doc/index.mld` (odoc entry point)
- `../specs/repos/ledger.md` (design summary, known gaps)
- `../oxcaml-claude-skills/AGENTS.md` and `../oxcaml-claude-skills/GOTCHAS.md`
- Skills in `../oxcaml-claude-skills/skills/`: `banking-standards`, `janestreet-ocaml-style`,
  `oxcaml-modes`

## Never do

- Run `opam install`, `upgrade` or `pin` that would change `ocaml-variants`, `core`, `base`,
  `async` or `ppx_jane` on the shared `5.2.0+ox` switch.
- Publish, tag or release.
- Regenerate or edit a compatibility fixture under `test/data`.
- Poll CI in a loop.
