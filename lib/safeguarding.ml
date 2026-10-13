open! Core
module Amount = Money.Amount
module Currency = Money.Currency

module Status = struct
  type t =
    | Balanced
    | Shortfall
    | Excess
  [@@deriving sexp_of, compare, equal]
end

module Recon = struct
  type t =
    { currency : Currency.t
    ; business_date : Date.t
    ; requirement : Amount.t
    ; internal_resource : Amount.t
    ; external_balance : Amount.t
    ; resource_minus_requirement : Amount.t
    ; external_minus_internal : Amount.t
    ; status : Status.t
    ; proposed_top_up : Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Exposure = struct
  type t =
    { currency : Currency.t
    ; customer_liability : Amount.t
    ; assets_held : Amount.t
    ; house_position : Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

let total balances (purpose : Account.Purpose.t) currency =
  Balances.accounts balances
  |> List.filter ~f:(fun (a : Account.t) -> Account.Purpose.equal a.purpose purpose)
  |> List.map ~f:(fun (a : Account.t) -> Balances.posted balances a.id currency)
  |> Amount.sum currency
  |> Or_error.ok_exn
;;

let reconcile balances ~currency ~business_date ~external_balance =
  if not (Currency.equal (Amount.currency external_balance) currency)
  then Or_error.error_s [%message "external balance in another currency"]
  else (
    let requirement = total balances Customer_funds currency in
    let internal_resource = total balances Safeguarding_asset currency in
    let diff = Amount.sub_exn internal_resource requirement in
    let status : Status.t =
      match Amount.sign diff with
      | 0 -> Balanced
      | s when s < 0 -> Shortfall
      | _ -> Excess
    in
    Ok
      { Recon.currency
      ; business_date
      ; requirement
      ; internal_resource
      ; external_balance
      ; resource_minus_requirement = diff
      ; external_minus_internal = Amount.sub_exn external_balance internal_resource
      ; status
      ; proposed_top_up =
          (match status with
           | Shortfall -> Amount.neg diff
           | Balanced | Excess -> Amount.zero currency)
      })
;;

let exposures balances =
  Balances.accounts balances
  |> List.filter ~f:(fun (a : Account.t) ->
    match a.purpose with
    | Customer_funds | Safeguarding_asset -> true
    | _ -> false)
  |> List.concat_map ~f:(fun (a : Account.t) -> Balances.currencies balances a.id)
  |> List.dedup_and_sort ~compare:Currency.compare
  |> List.map ~f:(fun currency ->
    let customer_liability = total balances Customer_funds currency in
    let assets_held = total balances Safeguarding_asset currency in
    { Exposure.currency
    ; customer_liability
    ; assets_held
    ; house_position = Amount.sub_exn assets_held customer_liability
    })
;;
