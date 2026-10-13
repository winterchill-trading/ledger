open! Core

module type S = sig
  type t [@@deriving quickcheck]

  include Identifiable.S with type t := t

  val of_string_or_error : string -> t Or_error.t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

let max_length = 128

let allowed_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | ':' | '/' | '@' | '+' -> true
  | _ -> false
;;

let check s =
  let len = String.length s in
  if len = 0
  then Or_error.error_string "empty id"
  else if len > max_length
  then Or_error.error_s [%message "id too long" (len : int) (max_length : int)]
  else (
    match String.find s ~f:(Fn.non allowed_char) with
    | None -> Ok ()
    | Some c -> Or_error.error_s [%message "invalid character in id" (c : char) s])
;;

module Make
    (M : sig
       val module_name : string
     end)
    () =
struct
  let of_string_or_error s =
    Or_error.tag_s
      (Or_error.map (check s) ~f:(fun () -> s))
      ~tag:[%message M.module_name ~id:(s : string)]
  ;;

  module T = struct
    type t = string [@@deriving bin_io, compare, equal, hash]

    let module_name = M.module_name
    let to_string t = t
    let of_string s = Or_error.ok_exn (of_string_or_error s)
    let sexp_of_t = sexp_of_string
    let t_of_sexp sexp = of_string (string_of_sexp sexp)
  end

  include T
  include Identifiable.Make (T)

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind len = Int.gen_incl 1 12 in
    String.gen_with_length
      len
      (Quickcheck.Generator.of_list (String.to_list "abcxyz09:-_"))
  ;;

  let quickcheck_observer = String.quickcheck_observer
  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

  module Stable = struct
    module V1 = T
  end
end
