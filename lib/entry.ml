open! Core

module Kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Transfer of string
        | Hold_place of Hold_id.Stable.V1.t
        | Hold_release of Hold_id.Stable.V1.t
        | Hold_capture of Hold_id.Stable.V1.t
        | Reversal of { reverses : int }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Repr = struct
  type t =
    { idempotency_key : Idempotency_key.Stable.V1.t
    ; kind : Kind.Stable.V1.t
    ; reference : string
    ; effective_at : Time_ns.Stable.Alternate_sexp.V1.t
    ; recorded_at : Time_ns.Stable.Alternate_sexp.V1.t
    ; postings : Posting.Stable.V1.t list
    }
  [@@deriving bin_io, sexp, compare, equal]
end

include Repr

let validate (t : t) =
  let open Or_error.Let_syntax in
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

let create ~idempotency_key ~kind ?(reference = "") ~effective_at ~recorded_at postings =
  let t = { idempotency_key; kind; reference; effective_at; recorded_at; postings } in
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

let same_request a b = equal a { b with recorded_at = a.recorded_at }

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
