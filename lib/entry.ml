open! Core

module Code = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Deposit
        | Withdrawal
        | Transfer_intraledger
        | Fx_leg
        | Fee
        | Card_auth
        | Card_settlement
        | Refund
        | Reversal
        | Return
        | Recall
        | Adjustment of
            { maker : string
            ; checker : string
            ; reason : string
            ; ticket : string
            }
        | Interest
        | Safeguarding_sweep
        | Hold
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let to_string = function
    | Adjustment _ -> "ADJUSTMENT"
    | t -> String.uppercase (Sexp.to_string (sexp_of_t t))
  ;;
end

module Kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Transfer of string
        | Hold_place of Hold_id.Stable.V1.t
        | Hold_release of Hold_id.Stable.V1.t
        | Hold_capture of Hold_id.Stable.V1.t
        | Reversal of { reverses : Idempotency_key.Stable.V1.t }
        | Hold_place_expiring of
            { hold : Hold_id.Stable.V1.t
            ; expires_at : Time_ns.Stable.Alternate_sexp.V1.t
            }
        | Hold_expire of Hold_id.Stable.V1.t
        | Hold_over_capture of
            { hold : Hold_id.Stable.V1.t
            ; reason : string
            }
        | Hold_increment of Hold_id.Stable.V1.t
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let hold = function
    | Transfer _ | Reversal _ -> None
    | Hold_place hold
    | Hold_release hold
    | Hold_capture hold
    | Hold_expire hold
    | Hold_increment hold
    | Hold_place_expiring { hold; _ }
    | Hold_over_capture { hold; _ } -> Some hold
  ;;
end

module Repr = struct
  type t =
    { idempotency_key : Idempotency_key.Stable.V1.t
    ; kind : Kind.Stable.V1.t
    ; code : Code.Stable.V1.t
    ; reference : string
    ; remittance_info : string
    ; effective_at : Time_ns.Stable.Alternate_sexp.V1.t
    ; recorded_at : Time_ns.Stable.Alternate_sexp.V1.t
    ; business_date : Date.Stable.V1.t
    ; sca : Sca_evidence.Stable.V1.t option
    ; postings : Posting.Stable.V1.t list
    }
  [@@deriving bin_io, sexp, compare, equal]
end

include Repr

let validate (t : t) =
  let open Or_error.Let_syntax in
  let%bind () =
    match t.code with
    | Adjustment { maker; checker; reason; ticket } ->
      if List.exists [ maker; checker; reason; ticket ] ~f:String.is_empty
      then
        Or_error.error_s
          [%message "an adjustment needs a maker, a checker, a reason and a ticket"]
      else if String.equal maker checker
      then
        Or_error.error_s
          [%message "an adjustment's maker and checker must differ" maker checker]
      else Ok ()
    | _ -> Ok ()
  in
  match t.postings with
  | [] | [ _ ] ->
    Or_error.error_s
      [%message
        "an entry needs at least two postings"
          ~idempotency_key:(t.idempotency_key : Idempotency_key.t)]
  | first :: _ ->
    let currency = Money.Amount.currency first.amount in
    let%bind () =
      List.find_map t.postings ~f:(fun (p : Posting.t) ->
        if not (Money.Currency.equal (Money.Amount.currency p.amount) currency)
        then Some [%message "postings in more than one currency" (p : Posting.t)]
        else if not (Money.Amount.is_positive p.amount)
        then Some [%message "posting amount must be positive" (p : Posting.t)]
        else None)
      |> function
      | None -> Ok ()
      | Some msg ->
        Or_error.error_s
          [%message
            "" ~_:(msg : Sexp.t) ~idempotency_key:(t.idempotency_key : Idempotency_key.t)]
    in
    let net =
      List.fold t.postings ~init:(Money.Amount.zero currency) ~f:(fun acc p ->
        Money.Amount.add_exn acc (Posting.signed p))
    in
    if Money.Amount.is_zero net
    then Ok ()
    else
      Or_error.error_s
        [%message
          "unbalanced entry: debits minus credits is not zero"
            ~debits_minus_credits:(Money.Amount.to_string net)
            ~idempotency_key:(t.idempotency_key : Idempotency_key.t)]
;;

let utc_date time = Time_ns.to_date time ~zone:Time_float.Zone.utc

let default_code : Kind.t -> Code.t = function
  | Transfer _ -> Transfer_intraledger
  | Reversal _ -> Reversal
  | Hold_place _
  | Hold_release _
  | Hold_capture _
  | Hold_place_expiring _
  | Hold_expire _
  | Hold_increment _
  | Hold_over_capture _ -> Hold
;;

let create
  ~idempotency_key
  ~kind
  ?(code = default_code kind)
  ?(reference = "")
  ?(remittance_info = "")
  ?business_date
  ?sca
  ~effective_at
  ~recorded_at
  postings
  =
  let t =
    { idempotency_key
    ; kind
    ; code
    ; reference
    ; remittance_info
    ; effective_at
    ; recorded_at
    ; business_date = Option.value business_date ~default:(utc_date recorded_at)
    ; sca
    ; postings
    }
  in
  Or_error.map (validate t) ~f:(fun () -> t)
;;

let restamp t ~recorded_at ~business_date = { t with recorded_at; business_date }

let with_code t code =
  let t = { t with code } in
  Or_error.map (validate t) ~f:(fun () -> t)
;;

(* [validate] guarantees at least two postings. *)
let currency t = Money.Amount.currency (List.hd_exn t.postings).amount

let total t =
  List.fold
    t.postings
    ~init:(Money.Amount.zero (currency t))
    ~f:(fun acc p ->
      match p.side with
      | Debit -> Money.Amount.add_exn acc p.amount
      | Credit -> acc)
;;

let same_request a b =
  equal a { b with recorded_at = a.recorded_at; business_date = a.business_date }
;;

module Stable = struct
  module V1 = struct
    include Repr

    let of_repr t = Or_error.ok_exn (Or_error.map (validate t) ~f:(fun () -> t))
    let t_of_sexp sexp = of_repr (Repr.t_of_sexp sexp)

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type t = Repr.t

          let to_binable = Fn.id
          let of_binable = of_repr
        end)
  end
end
