(lang dune 3.17)

(name ledger)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/ledger))

(package
 (name ledger)
 (synopsis "Double-entry ledger over an append-only Bin_prot journal")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  ppx_jane))
