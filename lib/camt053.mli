(** A bank-to-customer statement in the shape of ISO 20022 camt.053 (and, with pending
    entries, camt.052), as OCaml values: no XML. A serializer maps fields one to one:

    {v
    t.statement_id          Stmt/Id
    t.created_at            Stmt/CreDtTm
    t.from_date, to_date    Stmt/FrToDt
    t.account, currency     Stmt/Acct/Id/Othr/Id, Stmt/Acct/Ccy
    Balance.type_           Stmt/Bal/Tp/CdOrPrtry/Cd (OPBD, CLBD, CLAV)
    Balance.amount          Stmt/Bal/Amt (@Ccy)
    Balance.indicator       Stmt/Bal/CdtDbtInd (CRDT, DBIT)
    Balance.date            Stmt/Bal/Dt/Dt
    Entry.entry_ref         Ntry/NtryRef
    Entry.amount            Ntry/Amt
    Entry.indicator         Ntry/CdtDbtInd
    Entry.reversal          Ntry/RvslInd
    Entry.status            Ntry/Sts/Cd (BOOK, PDNG)
    Entry.booking_date      Ntry/BookgDt/Dt
    Entry.value_date        Ntry/ValDt/Dt
    Entry.account_servicer_ref  Ntry/AcctSvcrRef
    Entry.bank_transaction_code Ntry/BkTxCd/Prtry/Cd
    Entry.end_to_end_id     Ntry/NtryDtls/TxDtls/Refs/EndToEndId
    Entry.remittance_info   Ntry/NtryDtls/TxDtls/RmtInf/Ustrd
    v}

    Entries show what the account holder sees: an account and its pending sub-account
    together. Placing or releasing a hold is not a booked movement (it moves money between
    the two); a capture is, for the amount that left. Open holds appear as [Pdng] entries
    when [include_pending] is set. Amounts are always positive; the indicator says which
    way, from the ledger's side of the account: a credit to a customer (Liability)
    account, money in, is [Crdt]. *)

open! Core

module Indicator : sig
  type t =
    | Crdt
    | Dbit
  [@@deriving sexp_of, compare, equal]
end

module Status : sig
  type t =
    | Book
    | Pdng
  [@@deriving sexp_of, compare, equal]
end

module Balance : sig
  module Type : sig
    type t =
      | Opbd (** opening booked *)
      | Clbd (** closing booked *)
      | Clav (** closing available *)
    [@@deriving sexp_of, compare, equal]
  end

  type t =
    { type_ : Type.t
    ; amount : Money.Amount.t
    ; indicator : Indicator.t
    ; date : Date.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Entry : sig
  type t =
    { entry_ref : string (** [<seq>.<index>] in the journal *)
    ; amount : Money.Amount.t
    ; indicator : Indicator.t
    ; reversal : bool
    ; status : Status.t
    ; booking_date : Date.t option (** [None] for pending entries *)
    ; value_date : Date.t
    ; account_servicer_ref : string (** the ledger idempotency key *)
    ; bank_transaction_code : string (** {!Ledger.Entry.Code.to_string} *)
    ; end_to_end_id : string (** the entry's [reference] *)
    ; remittance_info : string
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { statement_id : string
  ; created_at : Time_ns.Alternate_sexp.t
  ; account : Account_id.t
  ; currency : Money.Currency.t
  ; from_date : Date.t
  ; to_date : Date.t
  ; balances : Balance.t list
  ; entries : Entry.t list
  }
[@@deriving sexp_of, compare, equal]

(** The statement of [account] in [currency] for business dates [from_date] to [to_date]
    inclusive, from a complete journal. [statement_id] defaults to
    [<account>/<currency>/<from_date>]. *)
val create
  :  ?include_pending:bool (** default [false] *)
  -> ?statement_id:string
  -> Record.t list
  -> account:Account_id.t
  -> currency:Money.Currency.t
  -> from_date:Date.t
  -> to_date:Date.t
  -> created_at:Time_ns.t
  -> t Or_error.t

val to_string_hum : t -> string
