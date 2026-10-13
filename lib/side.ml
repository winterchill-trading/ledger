open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Debit
      | Credit
    [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
  end
end

include Stable.V1

let flip = function
  | Debit -> Credit
  | Credit -> Debit
;;
