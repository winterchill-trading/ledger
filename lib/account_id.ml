open! Core

include
  Validated_id.Make
    (struct
      let module_name = "Ledger.Account_id"
    end)
    ()

let suffix = "/pending"
let max_parent_length = Validated_id.max_length - String.length suffix
let pending t = of_string (to_string t ^ suffix)
let is_pending t = String.is_suffix (to_string t) ~suffix

let parent_of_pending t =
  Option.map (String.chop_suffix (to_string t) ~suffix) ~f:of_string
;;
