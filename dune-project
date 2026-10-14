(lang dune 3.17)

(name ledger)

(generate_opam_files true)

(license MIT)

(authors "WinterChill AI")

(maintainers "hello@winterchill.xyz")

(source
 (github winterchill-trading/ledger))

(homepage "https://github.com/winterchill-trading/ledger")

(bug_reports "https://github.com/winterchill-trading/ledger/issues")

(documentation "https://github.com/winterchill-trading/ledger#readme")

(package
 (name ledger)
 (synopsis "Double-entry ledger over an append-only, hash-chained Bin_prot journal")
 (description
  "Ledger keeps double-entry books in exact integer minor units: balanced entries in one currency, two-phase holds (place, increment, capture, release, expire), reversals, four-eyes adjustments, idempotency keys with remembered refusals, and business-day closes. Every rule is enforced by one pure fold, so any accepted sequence of records keeps each currency balanced and restricted accounts above zero. Records are stored in an append-only file of checksummed, SHA-256 hash-chained Bin_prot frames, fsynced per append, with end-of-day snapshots and a chain head to publish for external anchoring. Reports include trial balance, paged account statements and camt.053-shaped statements as OCaml values. A command-line tool checks and prints journals.")
 (tags
  (ledger double-entry accounting bookkeeping journal idempotency banking))
 (depends
  (ocaml
   (>= 5.2))
  dune
  (core
   (>= v0.18~preview))
  (core_unix
   (>= v0.18~preview))
  (async_kernel
   (>= v0.18~preview))
  (ppx_jane
   (>= v0.18~preview))
  (cryptokit
   (>= 1.16))
  money
  (expect_test_helpers_core
   (and
    :with-test
    (>= v0.18~preview)))
  (odoc :with-doc)))
