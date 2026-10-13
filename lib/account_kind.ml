open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Asset
      | Liability
      | Equity
      | Income
      | Expense
    [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
  end
end

include Stable.V1

let normal_side : t -> Side.t = function
  | Asset | Expense -> Debit
  | Liability | Equity | Income -> Credit
;;
