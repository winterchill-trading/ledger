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

demo never overwrites an existing journal.

  $ ledger demo -journal j.bin
  (Unix.Unix_error "File exists" open
   "((filename j.bin) (mode (O_WRONLY O_CREAT O_EXCL O_APPEND)) (perm 0o644))")
  [1]

A damaged journal is reported, and reading stops at the damage.

  $ head -c 400 j.bin > cut.bin
  $ ledger trial-balance -journal cut.bin
  ("warning: journal does not end cleanly"
   (tail (Truncated (offset 373) (bytes 27))))
  ACCOUNT         CURRENCY                DEBIT               CREDIT
  bank:central    EUR                    250.00
  customer:alice  EUR                                         250.00
  TOTAL           EUR                    250.00               250.00
  $ ledger statement -journal j.bin -account customer:nobody
  ("account not opened" (account customer:nobody))
  [1]
