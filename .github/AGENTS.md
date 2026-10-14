# .github: ledger CI

`workflows/ci.yml` runs the ox-setup action with the pins `fixed-decimal` and `money`, then build,
tests, the quickstart example and the odoc build with warnings as errors.

Rules:

- Keep the pins in dependency order and in step with `dune-project`.
- Do not remove the odoc warning gate to make a build pass; fix the warning.
- To check a run: `gh run list --limit 1`, then at most one `gh run watch <id> --exit-status`.
