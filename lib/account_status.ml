open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Open
      | Frozen
      | Closed
    [@@deriving bin_io, sexp, compare, equal, enumerate]
  end
end

include Stable.V1
