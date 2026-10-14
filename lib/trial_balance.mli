(** A trial balance: every account's balance on its debit or credit column, with totals
    per currency. In a correct ledger the two columns of each currency are equal. *)

open! Core

module Row : sig
  type t =
    { account : Account_id.t
    ; currency : Money.Currency.t
    ; debit : Money.Amount.t (** zero when the balance is on the credit side *)
    ; credit : Money.Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Total : sig
  type t =
    { currency : Money.Currency.t
    ; debits : Money.Amount.t
    ; credits : Money.Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { rows : Row.t list (** sorted by account, then currency; zero balances left out *)
  ; totals : Total.t list (** sorted by currency *)
  }
[@@deriving sexp_of, compare, equal]

(** The debit and credit columns of every account and currency in the state. *)
val of_balances : Balances.t -> t

(** Debits equal credits in every currency. *)
val is_balanced : t -> bool

(** [Ok ()] when balanced, otherwise an error naming each unbalanced currency. *)
val check : t -> unit Or_error.t

(** A fixed-width table, one row per account and currency, then the totals. *)
val to_string_hum : t -> string
