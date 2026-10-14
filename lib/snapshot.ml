open! Core
open Money

module Balance = struct
  module Stable = struct
    module V1 = struct
      type t =
        { location : Location.Stable.V1.t
        ; amount : Amount.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    { location : Location.t
    ; amount : Amount.t
    }
  [@@deriving sexp, compare, equal]
end

module Stable = struct
  module V1 = struct
    type t =
      { at : Time_ns.Stable.Alternate_sexp.V1.t
      ; balances : Balance.Stable.V1.t list
      ; prices : Rate.Stable.V1.t list
      ; price_history : Rate.Stable.V1.t list [@sexp.list]
      ; runs : Run_record.Stable.V1.t list [@sexp.list]
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { at : Time_ns.Alternate_sexp.t
  ; balances : Balance.t list
  ; prices : Rate.t list
  ; price_history : Rate.t list [@sexp.list]
  ; runs : Run_record.t list [@sexp.list]
  }
[@@deriving sexp, compare, equal]

let create ~at ?(balances = []) ?(prices = []) ?(price_history = []) ?(runs = []) () =
  { at; balances; prices; price_history; runs }
;;

let balance t location currency =
  List.filter t.balances ~f:(fun (b : Balance.t) ->
    Location.equal b.location location && Currency.equal (Amount.currency b.amount) currency)
  |> function
  | [] -> None
  | rows -> Some (Amount.sum currency (List.map rows ~f:(fun (b : Balance.t) -> b.amount)) |> Or_error.ok_exn)
;;

let newest t ~base ~quote ~now ~max_age =
  let oldest_allowed = Time_ns.sub now max_age in
  List.filter t.prices ~f:(fun r ->
    Currency.equal (Rate.base r) base
    && Currency.equal (Rate.quote r) quote
    && Time_ns.( >= ) (Rate.at r) oldest_allowed)
  |> List.max_elt ~compare:(fun a b -> Time_ns.compare (Rate.at a) (Rate.at b))
;;

let direct t ~base ~quote ~now ~max_age =
  match newest t ~base ~quote ~now ~max_age with
  | Some r -> Some r
  | None ->
    Option.map (newest t ~base:quote ~quote:base ~now ~max_age) ~f:Rate.inverse
;;

let rate t ~base ~quote ~via ~now ~max_age =
  if Currency.equal base quote
  then None
  else (
    match direct t ~base ~quote ~now ~max_age with
    | Some r -> Some r
    | None ->
      if Currency.equal via base || Currency.equal via quote
      then None
      else (
        match
          direct t ~base ~quote:via ~now ~max_age, direct t ~base:via ~quote ~now ~max_age
        with
        | Some a, Some b ->
          Rate.create
            ~base
            ~quote
            ~price:(Q.mul (Rate.price a) (Rate.price b))
            ~at:(Time_ns.min (Rate.at a) (Rate.at b))
            ~source:(sprintf "cross via %s" (Currency.to_string via))
          |> Or_error.ok
        | _ -> None))
;;
