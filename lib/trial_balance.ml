open! Core
module Amount = Money.Amount
module Currency = Money.Currency

module Row = struct
  type t =
    { account : Account_id.t
    ; currency : Currency.t
    ; debit : Amount.t
    ; credit : Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Total = struct
  type t =
    { currency : Currency.t
    ; debits : Amount.t
    ; credits : Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { rows : Row.t list
  ; totals : Total.t list
  }
[@@deriving sexp_of, compare, equal]

let of_balances balances =
  let rows =
    Balances.debits_minus_credits balances
    |> List.filter_map ~f:(fun (account, currency, signed) ->
      let zero = Amount.zero currency in
      if Amount.is_zero signed
      then None
      else if Amount.is_positive signed
      then Some { Row.account; currency; debit = signed; credit = zero }
      else Some { Row.account; currency; debit = zero; credit = Amount.neg signed })
  in
  let totals =
    List.fold rows ~init:Currency.Map.empty ~f:(fun acc (row : Row.t) ->
      Map.update acc row.currency ~f:(fun prev ->
        let prev =
          Option.value
            prev
            ~default:
              { Total.currency = row.currency
              ; debits = Amount.zero row.currency
              ; credits = Amount.zero row.currency
              }
        in
        { prev with
          debits = Amount.add_exn prev.debits row.debit
        ; credits = Amount.add_exn prev.credits row.credit
        }))
    |> Map.data
  in
  { rows; totals }
;;

let unbalanced t =
  List.filter t.totals ~f:(fun (total : Total.t) ->
    not (Amount.equal total.debits total.credits))
;;

let is_balanced t = List.is_empty (unbalanced t)

let check t =
  match unbalanced t with
  | [] -> Ok ()
  | totals ->
    Or_error.error_s [%message "trial balance does not balance" (totals : Total.t list)]
;;

let to_string_hum t =
  let account_width =
    List.fold t.rows ~init:(String.length "TOTAL") ~f:(fun w (r : Row.t) ->
      Int.max w (String.length (Account_id.to_string r.account)))
  in
  let cell a = if Amount.is_zero a then "" else Amount.to_decimal_string a in
  let line name currency debit credit =
    sprintf "%-*s  %-8s %20s %20s" account_width name currency debit credit
  in
  let header = line "ACCOUNT" "CURRENCY" "DEBIT" "CREDIT" in
  let rows =
    List.map t.rows ~f:(fun (r : Row.t) ->
      line
        (Account_id.to_string r.account)
        (Currency.to_string r.currency)
        (cell r.debit)
        (cell r.credit))
  in
  let totals =
    List.map t.totals ~f:(fun (total : Total.t) ->
      line
        "TOTAL"
        (Currency.to_string total.currency)
        (Amount.to_decimal_string total.debits)
        (Amount.to_decimal_string total.credits))
  in
  String.concat ~sep:"\n" ((header :: rows) @ totals) ^ "\n"
;;
