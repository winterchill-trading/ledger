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
| `Account` | id, kind, `allow_negative`, free `tags` map |
| `Posting` | account, Debit/Credit, positive `Money.Amount.t` |
| `Entry` | balanced postings in one currency; `idempotency_key`, `effective_at`, `recorded_at`, `reference`, `kind` |
| `Balances` | the state the journal folds to; `apply` enforces every rule |
| `Hold` | builds place / capture / release entries on `<account>/pending` |
| `Reversal` | builds the mirror entry of a transfer, linked by sequence number |
| `Trial_balance` | debit and credit columns per currency, `check` |
| `Statement` | postings to one account with a running balance, paging by sequence |
| `Record`, `Framing`, `Journal` | the durable, sequenced, checksummed file |

Rules `Balances.apply` enforces, so they hold after any sequence of accepted records:

- every entry balances (debits equal credits) in one currency, so each currency's trial
  balance is zero;
- balances are reported on the account's normal side; customer accounts are
  Liabilities, so a deposit credits them and reads as a positive balance;
- only accounts with `allow_negative` (settlement and external accounts) may go below
  zero; every other account, including every pending sub-account, refuses an entry or
  hold that would overdraw it;
- `available = posted - held`: a hold moves funds to the account's pending sub-account,
  and the pending balance always equals the open holds' remaining amounts;
- an idempotency key is booked once. `Journal` turns a retry of the same request into
  `Duplicate { seq }` and a reuse of the key for a different entry into an error;
- only transfers can be reversed, once each; holds are released or captured instead;
- records carry consecutive sequence numbers starting at 1.

`effective_at` is when the event happened (the card transaction date); `recorded_at` is
when the ledger booked it, stamped from the journal's `Time_source` (injectable, so tests
use a synthetic clock).

## Journal file

```
header : "LEDGJRN1" ++ 32-hex bin_shape digest of Record.Stable.V1.t
frame* : length (8 bytes LE) ++ complemented length (8) ++ MD5 of payload (16) ++ bin_io record
```

- Each append is validated, written as one frame and `fsync`ed before it returns
  (`~fsync:false` for tests). One writer per file, enforced with `lockf`.
- On open the whole file is replayed: a damaged frame, a sequence gap or a record that
  breaks a rule is an error. A partial last frame (a crash during an append, which never
  returned success) is cut off only with `~repair_truncated_tail:true`.
- The stored complemented length means a damaged length header in the middle of the file
  is reported as corruption, never mistaken for a truncated tail that a repair would cut
  (taking the good frames after it).
- Readers accept the digests in `Record.compatible_digests` (append-only, pinned in a
  test); see `record.mli` for how to change a record type.

## Tests

`dune test` runs expect tests (entries, balances, holds, reversals, journal reopen,
truncation and repair, corruption, locking, a printed statement, paging), a cram test of
the CLI and Quickcheck properties:

- any sequence of random operations (deposits, transfers, withdrawals, holds, captures,
  releases, reversals; many invalid on purpose) keeps every currency balanced, no
  restricted account below zero, and held funds equal to open holds;
- replaying the accepted records, in memory and through a journal file, reproduces the
  same balances and holds.

## Known gaps

- The journal is read into memory on open; fine for a demo bank, not for years of a
  real one (snapshots of `Balances` would be the next step).
- `fsync` on macOS does not force the drive cache (`F_FULLFSYNC`); Linux `fsync` does.
- Entries are single-currency. An FX conversion is two entries through a conversion
  account per currency, linked by `reference`.
- No account closing or freezing yet; those belong to the layers above for now.
