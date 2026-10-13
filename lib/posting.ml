open! Core

module Stable = struct
  module V1 = struct
    type t =
      { account : Account_id.Stable.V1.t
      ; side : Side.Stable.V1.t
      ; amount : Money.Amount.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let debit account amount = { account; side = Debit; amount }
let credit account amount = { account; side = Credit; amount }
let flip t = { t with side = Side.flip t.side }

let signed t =
  match t.side with
  | Debit -> t.amount
  | Credit -> Money.Amount.neg t.amount
;;
