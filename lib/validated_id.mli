(** String identifiers that are checked once, at the edge.

    An id is 1 to 128 characters from [A-Z a-z 0-9 _ - . : / @ +]. That covers provider
    transaction ids, UUIDs, IBAN-like references and the [customer:42/eur] style used for
    account ids, while keeping ids safe to print in a statement, a file name or a log line
    without quoting. *)

open! Core

module type S = sig
  type t [@@deriving quickcheck]

  include Identifiable.S with type t := t

  (** [of_string] raises on an invalid id; this is the non-raising form. *)
  val of_string_or_error : string -> t Or_error.t

  module Stable : sig
    module V1 : sig
      (** Bin_prot is the plain string, so the encoding never changes with the validation
          rules. *)
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

(** 128 characters. *)
val max_length : int

(** Why [s] is not a valid id, if it is not. *)
val check : string -> unit Or_error.t

(** One fresh id type per application ([Account_id], [Hold_id], ...), so they cannot be
    mixed up. *)
module Make
    (_ : sig
       (** Used in error messages, e.g. ["Ledger.Account_id"]. *)
       val module_name : string
     end)
    () : S
