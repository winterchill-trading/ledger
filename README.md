# ledger

A double-entry ledger in OCaml: balanced journal entries, holds, reversals, trial
balance and account statements, stored in an append-only, checksummed Bin_prot journal.
Built for a bank or a banking demo, and meant to be usable later by an exchange
clearinghouse.

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
dune exec ledger -- demo -journal /tmp/demo.bin
dune exec ledger -- trial-balance -journal /tmp/demo.bin
dune exec ledger -- statement -journal /tmp/demo.bin -account customer:alice -currency EUR
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
balance, and both commands warn on stderr when the journal does not end cleanly.

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
  changed (use a new key for a new attempt).
- `Linked` entries (an FX swap's two legs and its fee) book together or not at all.
- Corrections are never edits: transfers are reversed (once, by key), holds are
  released, and manual fixes are `Adjustment`s that a second operator approves within
  24 hours (`Journal.propose_adjustment` / `approve_adjustment`). The `Adjustment` code
  is refused anywhere else.
- Holds may expire (`~timeout`); an expired hold cannot be captured or released, and
  `Journal.expire_holds` returns the rest. Captures can be partial and repeated, split
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
  MD5, breaks the next link. The head hash (`ledger verify`, and every snapshot) is the
  value to keep outside the system to cover the last frame too.
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

## Known gaps

- The journal is read into memory on open; snapshots save the fold, not the read. Years
  of a real bank would want segment files per period.
- `fsync` on macOS does not force the drive cache (`F_FULLFSYNC`); Linux `fsync` does.
- Hold expiry needs someone to call `Journal.expire_holds` (a timer, and `close_day`
  does it); between expiry and the sweep the funds stay held but cannot be captured.
- Refused keys are kept forever (the research note leaves a retention window open).
- Errors raised before an entry exists (a builder given an unknown hold id) are not
  recorded as refusals.
- Reconciliation breaks against provider statements (matching, ageing, resolution) are
  not here yet; `Clearing` and `Safeguarding` give the ledger side.
- Only the last frame relies on an external anchor for tamper evidence; nothing yet
  publishes the head hash anywhere.
