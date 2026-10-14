# ledger

A double-entry ledger in OCaml: balanced journal entries, two-phase holds, reversals,
four-eyes adjustments, trial balance and account statements, stored in an append-only,
checksummed and hash-chained Bin_prot journal. It suits anything that moves money on
behalf of others and must answer "what does this account hold, and did this event
already get booked": a payment or e-money institution, a card processor, a wallet, the
clearing side of an exchange.

Amounts come from [`money`](https://github.com/winterchill-trading/money): exact integer
minor units per currency (EUR 2 decimals, BTC 8, ETH 18 without truncation). There are
no floats anywhere.

## Why separate

Every money-moving component (payments, cards, risk checks, provider adapters) needs the
same answers: what does this account hold, what is reserved, did this event already get
booked, do the books balance. Putting those rules in one small library with no provider
or transport code means they are tested once, with properties, and every layer above
gets them for free. The ledger knows nothing about IBANs, cards or webhooks; it knows
accounts, postings and idempotency keys.

## Quick start

```sh
dune build
dune exec ledger -- example -journal /tmp/example.bin
dune exec ledger -- trial-balance -journal /tmp/example.bin
dune exec ledger -- statement -journal /tmp/example.bin -account customer:alice -currency EUR
dune exec ./examples/quickstart.exe
```

```
ACCOUNT         CURRENCY                DEBIT               CREDIT
bank:central    EUR                    250.00
customer:alice  EUR                                         230.10
external:cards  EUR                                          18.40
income:fees     EUR                                           1.50
TOTAL           EUR                    250.00               250.00
```

`statement` also takes `-from`, `-until` (UTC date or time, on `effective_at`),
`-after SEQ` and `-limit N` for paging. `trial-balance` exits 2 if the books do not
balance, and both commands warn on stderr when the journal does not end cleanly. Both
read the journal one frame at a time. `verify` checks a whole journal and prints its
chain head; `verify -head ledger-head-v1:...` checks that a head published earlier is
still in the file.

## Usage

```ocaml
open! Core
open Ledger

let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn
let id = Account_id.of_string
let key = Idempotency_key.of_string

let () =
  let j = Journal.create "bank.journal" |> Or_error.ok_exn in
  let ok r = ignore (Or_error.ok_exn r : Journal.Append_result.t) in
  ok (Journal.open_account j (Account.create (id "bank:central") Asset));
  ok (Journal.open_account j (Account.create (id "external:cards") Asset ~allow_negative:true));
  ok (Journal.open_account j (Account.create (id "customer:alice") Liability));
  (* A SEPA deposit, booked once even if the webhook arrives twice. *)
  ok
    (Journal.transfer j ~idempotency_key:(key "provider-event-123") ~tag:"deposit"
       ~effective_at:(Journal.now j)
       [ Posting.debit (id "bank:central") (eur "100.00")
       ; Posting.credit (id "customer:alice") (eur "100.00")
       ]);
  (* A card authorisation reserves funds; the settlement captures them. *)
  let auth = Hold_id.of_string "auth-9f2" in
  ok
    (Journal.hold j ~hold_id:auth ~account:(id "customer:alice") ~amount:(eur "12.00")
       ~idempotency_key:(key "auth-9f2") ~effective_at:(Journal.now j) ());
  ok
    (Journal.capture j ~hold_id:auth ~to_:(id "external:cards")
       ~idempotency_key:(key "settle-9f2") ~effective_at:(Journal.now j) ());
  let b = Journal.balances j in
  print_endline
    (Money.Amount.to_string (Balances.available b (id "customer:alice") Money.Currency.eur));
  Trial_balance.of_balances b |> Trial_balance.check |> Or_error.ok_exn;
  Journal.close j
;;
```

## Model

| Module | What it is |
|---|---|
| `Account_id`, `Hold_id`, `Idempotency_key` | Validated string ids (1-128 of `A-Za-z0-9_-.:/@+`), distinct types |
| `Account_kind` | Asset, Liability, Equity, Income, Expense, with their normal side |
| `Account` | id, kind, `purpose` (customer funds, safeguarding asset, clearing, ...), `allow_negative`, `tags` |
| `Account_status` | Open, Frozen (money in only), Closed (final, needs zero balance) |
| `Posting` | account, Debit/Credit, positive `Money.Amount.t` |
| `Entry` | balanced postings in one currency; idempotency key, `Code` (why), `Kind` (how), value date (`effective_at`), `recorded_at`, `business_date`, reference, remittance info, SCA evidence |
| `Balances` | the state the journal folds to; `apply` enforces every rule; posted/pending columns |
| `Hold` | two-phase transfers: place, increment, capture (split, release the rest), over-capture, release, expire |
| `Reversal` | the mirror entry of a transfer, linked by idempotency key |
| `Trial_balance` | debit and credit columns per currency, `check` |
| `Statement` | postings to one account with a running balance, paging by sequence |
| `Camt053` | ISO 20022 camt.053-shaped statement as OCaml values (no XML), field-to-element map in the `.mli` |
| `Clearing` | clearing accounts that did not return to zero, with age in business days |
| `Safeguarding` | per-currency requirement vs. safeguarded assets vs. external statement; exposure |
| `Record`, `Framing`, `Journal` | the durable, sequenced, checksummed, hash-chained file |
| `Chain_head` | the head of the hash chain as one line of text, to publish and verify later |
| `Snapshot` | end-of-day state and balance rows, tied to the journal bytes and chain head it covers |

### Rules (enforced by `Balances.apply`, so they hold after any accepted sequence)

- Every entry balances (debits equal credits) in one currency; each currency's trial
  balance is zero.
- Only accounts with `allow_negative` (settlement, external, clearing) may go below zero;
  every other account refuses an entry or hold that would overdraw it, counting held
  funds. This is TigerBeetle's `debits_must_not_exceed_credits` (or the credit
  equivalent) on the account's normal side.
- `available = posted - held`: a hold moves funds to `<account>/pending`; the pending
  balance always equals the open holds' remaining amounts.
- An idempotency key is used once, forever. A retry with the same content is
  `Duplicate { seq }`; different content is an error; an entry the rules refused is
  stored as a `Rejected` record and a retry gets the same refusal even if the state has
  changed (use a new key for a new attempt). That includes refusals by the builders
  before an entry exists (an unknown hold id). With `~refusal_retention:span` a refusal
  lasts `span`; after that a request under the key is new again, and `close_day` drops
  every refusal past the window (a `Refusals_forgotten` record, so replay agrees).
- `Linked` entries (an FX swap's two legs and its fee) book together or not at all.
- Corrections are never edits: transfers are reversed (once, by key), holds are
  released, and manual fixes are `Adjustment`s that a second operator approves within
  24 hours (`Journal.propose_adjustment` / `approve_adjustment`). The `Adjustment` code
  is refused anywhere else.
- Holds may expire (`~timeout`); an expired hold cannot be captured or released, and
  `Journal.expire_holds_at` returns the rest. `Journal.next_expiry` says when to call it
  next, so a caller can drive expiry from any timer (`close_day` also does it). Captures can be partial and repeated, split
  between payees, release the remainder in the same entry, or exceed the hold with a
  reason (`over_capture`, the excess posted from the account).
- The ledger assigns `recorded_at` (strictly increasing, even if the clock steps back)
  and `business_date`. `Journal.close_day` appends `Day_closed`; nothing is booked on a
  closed date afterwards, late events are booked on the next business date with their
  original value date.
- Frozen accounts take money in and return held funds, nothing else; closing needs a
  zero balance in every currency and is final.

### TigerBeetle mapping

| TigerBeetle | here |
|---|---|
| transfer `id` | `Entry.idempotency_key` (also the reversal and hold references) |
| `debits_pending`, `credits_pending`, `debits_posted`, `credits_posted` | `Balances.columns` |
| `ledger` | the currency of an entry |
| `code` | `Entry.Code.t` (plus a free tag in `Transfer`) |
| pending / post_pending / void_pending | `Hold.place` / `Hold.capture` / `Hold.release` |
| `timeout` | `Journal.hold ~timeout`, `Journal.expire_holds` |
| `linked` | `Journal.append_linked` |
| account flags `debits_must_not_exceed_credits` | `allow_negative = false` on a credit-normal account |
| `closed` account flag | `Account_status.Closed` |
| `exists`, `exists_with_different_*`, `id_already_failed` | `Duplicate`, reuse error, `Rejected` records |
| cluster `timestamp` | `recorded_at`, assigned by `Journal` |

Differences: an entry may have more than two postings (a capture split between a
merchant and a fee account, an FX leg with its fee), and a hold's payee is chosen at
capture time, not when the hold is placed.

## Journal file

```
header : "LEDGJRN1" ++ 32-hex bin_shape digest of Record.Stable.V1.t
frame* : length (8 bytes LE) ++ complemented length (8) ++ MD5 of payload (16)
         ++ SHA-256 of the previous frame's payload (32) ++ bin_io record
```

- Each append is validated, written as one frame and `fsync`ed before it returns. One
  writer per file (`lockf`).
- Opening replays the file (or the part after a snapshot): a damaged frame, a broken
  hash link, a sequence gap or a record breaking a rule is an error; a partial last
  frame from a crash is cut only with `~repair_truncated_tail:true`.
- The complemented length makes a damaged length header in the middle read as
  corruption, never as a truncated tail that a repair would cut.
- The SHA-256 links make the file a hash chain: rewriting any frame, even with a correct
  MD5, breaks the next link. `Journal.chain_head` (also `ledger verify` and every
  snapshot, `Chain_head.of_snapshot`) is the value to keep outside the system:
  `Chain_head.to_string` is one line (`ledger-head-v1:<seq>:<length>:<sha256>`) to send
  to an auditor, a transparency log or a timestamping service, and
  `Chain_head.verify` fails if records up to that head were cut off or rewritten.
- Opening reads the file one frame at a time (`Framing.fold`), so memory is the folded
  state, not the file. The state keeps every booked entry (for idempotency), and by
  default the journal also keeps the list of records for `Journal.records`;
  `~keep_records:false` drops it and `records` reads the file again. `Journal.fold`
  streams a file for reports.
- Readers accept the digests in `Record.compatible_digests` (append-only, pinned in a
  test); see `record.mli` for how to change a record type.

## End of day and snapshots

`Journal.close_day` expires due holds, appends `Day_closed`, and writes
`<journal>.snapshots/<date>.snap`: the full ledger state, the day's rows per account and
currency (opening booked, debits, credits, closing booked, closing available, i.e.
camt.053 `OPBD`/`CLBD`/`CLAV`), the sequence number, length, MD5 and chain head of the
journal prefix it covers. `Journal.open_ ~snapshot` starts from it and replays only the
tail; `Snapshot.verify` (and `ledger verify -snapshot`) replays the prefix from scratch
and requires the same state and rows.

## Reports

- `Camt053.create` / `ledger camt053`: booked entries of an account and its pending
  sub-account together (placing or releasing a hold is not a movement; a capture is),
  booking date, value date, CRDT/DBIT, BOOK/PDNG (open holds), references, remittance
  information, proprietary bank transaction code from `Entry.Code`.
- `Clearing.open_items`: clearing accounts with a non-zero balance and since when.
- `Safeguarding.reconcile`: requirement (customer funds) against internal safeguarding
  assets and the external statement balance; `Balanced | Shortfall | Excess` and a
  proposed top-up. `Safeguarding.exposures`: liability, assets, house position.

The ledger stores ids, never personal data: name customer accounts by an opaque
customer id and keep names in KYC systems.

## Tests

`dune test` runs expect tests (entries, balances, holds, reversals, journal reopen,
truncation and repair, corruption, hash chain, locking, statements and paging, linked
groups, remembered refusals, ledger time, account status, adjustments, day close and
snapshots, camt.053, clearing and safeguarding), a cram test of the CLI and Quickcheck
properties:

- any sequence of random operations (deposits, transfers, withdrawals, holds,
  increments, captures, over-captures, releases, reversals; many invalid on purpose)
  keeps every currency balanced, no restricted account below zero, held funds equal to
  open holds, and the posted/pending columns consistent with the balances;
- through a journal file (with refusals recorded and random day closes), reading,
  replaying, reopening and reopening from the last snapshot all reproduce the live
  state exactly, and every snapshot verifies.

## Portability

The library uses no OxCaml-only language feature (no modes, unboxed types, `or_null`,
`[@zero_alloc]` or stack allocation), so its own code builds with OCaml 5.2 or later.
It depends on Jane Street's `v0.18~preview` packages (`core`, `core_unix`,
`async_kernel`, `ppx_jane`), which today are published only in the OxCaml opam
repository, and on `money`, whose `fixed-decimal` dependency needs the OxCaml compiler.
A plain OCaml build needs released `v0.17` Jane Street packages (untested) and a
`fixed-decimal` built without its `[@zero_alloc]` checks.

## Releasing

The version lives only in git tags, written into `ledger.opam` by `dune subst` at
release time; `CHANGES.md` collects the notes under `## Unreleased`. A release is the
`dune-release` flow: move the `Unreleased` notes under the new version, then
`dune-release tag`, `dune-release distrib`, `dune-release publish` and
`dune-release opam pkg`/`submit`. `money` (and `fixed-decimal`) must be released first.
Nothing has been released yet.

API docs are built with `dune build @doc` (odoc). CI fails on any odoc warning, so run it before tagging.

## Known gaps

- `fsync` on macOS does not force the drive cache (`F_FULLFSYNC`); Linux `fsync` does.
- The folded state keeps every booked entry, because idempotency needs them; memory grows
  with the number of entries (not with the file, which is no longer read whole). A bank
  running for years would want segment files per period with the key index on disk; that
  is a format decision, not a bounded change.
- Matching against provider statements (breaks, ageing, resolution) is not here;
  `Clearing` and `Safeguarding` give the ledger side. How breaks are matched and
  resolved needs its own design.
- `ledger camt053` still reads every record into memory, since the statement replays the
  whole journal for its balances.

## Used by

`payments-core`, `card-core`, `banking-provider` and `neobank-api` in the same
organization.
