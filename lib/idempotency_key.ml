open! Core

include
  Validated_id.Make
    (struct
      let module_name = "Ledger.Idempotency_key"
    end)
    ()
