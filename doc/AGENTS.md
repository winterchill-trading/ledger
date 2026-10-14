# doc: ledger documentation

`index.mld` is the odoc entry point; `dune` declares the `ledger` documentation package.

Rules:

- The build must not warn: CI treats odoc warnings as errors. Check with
  `dune build --root .. @ledger/doc` from the workspace.
- Reference the modules by their public names; a broken `{!...}` reference is a warning.
- Design decisions and the journal format belong in `../README.md` and `../../specs/repos/ledger.md`.
