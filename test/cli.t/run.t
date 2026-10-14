Write the example journal, then read it back.

  $ ledger demo -journal j.bin
  wrote j.bin
  $ ledger trial-balance -journal j.bin
  ACCOUNT         CURRENCY                DEBIT               CREDIT
  bank:central    EUR                    250.00
  customer:alice  EUR                                         230.10
  external:cards  EUR                                          18.40
  income:fees     EUR                                           1.50
  TOTAL           EUR                    250.00               250.00
  $ ledger statement -journal j.bin -account customer:alice
  Statement customer:alice EUR
    SEQ  EFFECTIVE (UTC)      KIND               REFERENCE               DEBIT         CREDIT        BALANCE
                              opening balance                                                           0.00
      5  2026-10-01 09:00:00  deposit            sepa-in-001                           250.00         250.00
      6  2026-10-02 08:15:00  hold auth-1        card-bakery             18.40                        231.60
      8  2026-10-31 23:00:00  fee                monthly-fee              1.50                        230.10
                              closing balance                                                         230.10
  $ ledger statement -journal j.bin -account customer:alice -from 2026-10-02 -limit 1
  Statement customer:alice EUR
    SEQ  EFFECTIVE (UTC)      KIND               REFERENCE               DEBIT         CREDIT        BALANCE
                              opening balance                                                         250.00
      6  2026-10-02 08:15:00  hold auth-1        card-bakery             18.40                        231.60
                              closing balance                                                         231.60
  more: -after 6

The same month as camt.053, with the open card authorisation as pending.

  $ ledger camt053 -journal j.bin -account customer:alice -from 2026-10-01 -to 2026-10-31 -pending
  camt.053 customer:alice/EUR/2026-10-01 customer:alice EUR 2026-10-01..2026-10-31
    OPBD 2026-10-01 CRDT 0.00 EUR
    CLBD 2026-10-31 CRDT 230.10 EUR
    CLAV 2026-10-31 CRDT 230.10 EUR
    5.0      BOOK 2026-10-01 2026-10-01 CRDT       250.00 EUR DEPOSIT           sepa-in-001
    7.0      BOOK 2026-10-01 2026-10-03 DBIT        18.40 EUR CARD_SETTLEMENT   card-bakery
    8.0      BOOK 2026-10-01 2026-10-31 DBIT         1.50 EUR FEE               monthly-fee

verify checks frames, the hash chain and the ledger rules, and prints the chain head
in the form to publish. A head published earlier must still be in the journal.

  $ ledger verify -journal j.bin
  ((records 8) (tail Clean)
   (head cc42d0eb7b8493b1ccbaacae881a0a0b75c17daa84d4e51476d68d78bb89d966)
   (chain_head
    ledger-head-v1:8:1067:cc42d0eb7b8493b1ccbaacae881a0a0b75c17daa84d4e51476d68d78bb89d966))

demo never overwrites an existing journal.

  $ ledger demo -journal j.bin
  (Unix.Unix_error "File exists" open
   "((filename j.bin) (mode (O_WRONLY O_CREAT O_EXCL O_APPEND)) (perm 0o644))")
  [1]

A damaged journal is reported, and reading stops at the damage.

  $ head -c 1000 j.bin > cut.bin
  $ ledger trial-balance -journal cut.bin
  ("warning: journal does not end cleanly"
   (tail (Truncated (offset 900) (bytes 100))))
  ACCOUNT         CURRENCY                DEBIT               CREDIT
  bank:central    EUR                    250.00
  customer:alice  EUR                                         231.60
  external:cards  EUR                                          18.40
  TOTAL           EUR                    250.00               250.00
  $ ledger verify -journal cut.bin
  ((records 7) (tail (Truncated (offset 900) (bytes 100)))
   (head f12a6d70772309e840fe55e6b9560912713fb00450715de06dedca6484b6a2da)
   (chain_head
    ledger-head-v1:7:900:f12a6d70772309e840fe55e6b9560912713fb00450715de06dedca6484b6a2da))
  [2]

A copy cut at a frame boundary is clean, but the head published for the whole journal
is not in it; the head of the shorter copy is in the whole journal.

  $ head -c 900 j.bin > short.bin
  $ ledger verify -journal short.bin -head ledger-head-v1:8:1067:cc42d0eb7b8493b1ccbaacae881a0a0b75c17daa84d4e51476d68d78bb89d966 2>&1 | grep -o 'does not contain the published head'
  does not contain the published head
  $ ledger verify -journal j.bin -head ledger-head-v1:7:900:f12a6d70772309e840fe55e6b9560912713fb00450715de06dedca6484b6a2da | tail -1
  head ok
  $ ledger statement -journal j.bin -account customer:nobody
  ("account not opened" (account customer:nobody))
  [1]
