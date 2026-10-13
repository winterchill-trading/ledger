open! Core
module Amount = Money.Amount
module Currency = Money.Currency

module Item = struct
  type t =
    { account : Account_id.t
    ; currency : Currency.t
    ; balance : Amount.t
    ; non_zero_since : Date.t
    ; age_days : int
    }
  [@@deriving sexp_of, compare, equal]
end

let open_items ?(older_than_days = 0) records ~as_of =
  let open Or_error.Let_syntax in
  let is_clearing state id =
    match Balances.find_account state id with
    | Some { purpose = Clearing; _ } -> true
    | _ -> false
  in
  (* For each clearing account and currency, the business date it last went from zero to
     non-zero. *)
  let%map state, since =
    List.fold_result
      records
      ~init:(Balances.empty, Map.Poly.empty)
      ~f:(fun (state, since) r ->
        let%map next = Balances.apply state r in
        let since =
          List.fold (Record.entries r) ~init:since ~f:(fun since (e : Entry.t) ->
            List.fold e.postings ~init:since ~f:(fun since p ->
              let id = p.account in
              if not (is_clearing next id)
              then since
              else (
                let currency = Entry.currency e in
                let before = Balances.posted state id currency in
                let after = Balances.posted next id currency in
                if Amount.is_zero after
                then Map.remove since (id, currency)
                else if Amount.is_zero before
                then Map.set since ~key:(id, currency) ~data:e.business_date
                else since)))
        in
        next, since)
  in
  Map.to_alist since
  |> List.filter_map ~f:(fun ((account, currency), non_zero_since) ->
    let balance = Balances.posted state account currency in
    let age_days = Date.diff as_of non_zero_since in
    if Amount.is_zero balance || age_days < older_than_days
    then None
    else Some { Item.account; currency; balance; non_zero_since; age_days })
  |> List.sort ~compare:(fun (a : Item.t) b ->
    [%compare: Date.t * Account_id.t]
      (a.non_zero_since, a.account)
      (b.non_zero_since, b.account))
;;
