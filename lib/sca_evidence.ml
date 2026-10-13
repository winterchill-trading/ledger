open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Challenge of string
      | Exemption of string
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1
