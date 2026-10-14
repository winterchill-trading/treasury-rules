open! Core
open Money

module Side = struct
  type t =
    | Buy
    | Sell
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Transfer of
            { from : Location.Stable.V1.t
            ; to_ : Location.Stable.V1.t
            ; amount : Amount.Stable.V1.t
            }
        | Convert of
            { from : Location.Stable.V1.t
            ; to_ : Location.Stable.V1.t
            ; sell : Amount.Stable.V1.t
            ; buy_estimate : Amount.Stable.V1.t
            ; rate : Rate.Stable.V1.t
            }
        | Subscribe of
            { from : Location.Stable.V1.t
            ; fund : Location.Stable.V1.t
            ; amount : Amount.Stable.V1.t
            }
        | Redeem of
            { fund : Location.Stable.V1.t
            ; to_ : Location.Stable.V1.t
            ; amount : Amount.Stable.V1.t
            }
        | Hedge of
            { position : Location.Stable.V1.t
            ; side : Side.t
            ; size : Amount.Stable.V1.t
            ; price : Rate.Stable.V1.t
            }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Transfer of
        { from : Location.t
        ; to_ : Location.t
        ; amount : Amount.t
        }
    | Convert of
        { from : Location.t
        ; to_ : Location.t
        ; sell : Amount.t
        ; buy_estimate : Amount.t
        ; rate : Rate.t
        }
    | Subscribe of
        { from : Location.t
        ; fund : Location.t
        ; amount : Amount.t
        }
    | Redeem of
        { fund : Location.t
        ; to_ : Location.t
        ; amount : Amount.t
        }
    | Hedge of
        { position : Location.t
        ; side : Side.t
        ; size : Amount.t
        ; price : Rate.t
        }
  [@@deriving sexp, compare, equal]
end

module Stable = struct
  module V1 = struct
    type t =
      { kind : Kind.Stable.V1.t
      ; reason : Reason_code.Stable.V1.t
      ; value : Amount.Stable.V1.t
      ; minimum : Amount.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { kind : Kind.t
  ; reason : Reason_code.t
  ; value : Amount.t
  ; minimum : Amount.t
  }
[@@deriving sexp, compare, equal]

let amount t =
  match t.kind with
  | Transfer { amount; _ } | Subscribe { amount; _ } | Redeem { amount; _ } -> amount
  | Convert { sell; _ } -> sell
  | Hedge { size; _ } -> size
;;

let down = Rounding.Down

let scale t ratio =
  let shrink a = Amount.mul_ratio ~rounding:down a ratio in
  let kind : Kind.t =
    match t.kind with
    | Transfer r -> Transfer { r with amount = shrink r.amount }
    | Subscribe r -> Subscribe { r with amount = shrink r.amount }
    | Redeem r -> Redeem { r with amount = shrink r.amount }
    | Hedge r -> Hedge { r with size = shrink r.size }
    | Convert r ->
      let sell = shrink r.sell in
      let buy_estimate =
        Rate.convert ~rounding:down r.rate sell |> Or_error.ok_exn
      in
      Convert { r with sell; buy_estimate }
  in
  let t' = { t with kind; value = shrink t.value } in
  let a = amount t' in
  if Amount.is_positive a && Amount.( >= ) a t.minimum then Some t' else None
;;

let to_string_hum t =
  let a = Amount.to_string in
  let l = Location.to_string in
  match t.kind with
  | Transfer { from; to_; amount } ->
    sprintf "transfer %s %s -> %s" (a amount) (l from) (l to_)
  | Convert { from; to_; sell; buy_estimate; rate } ->
    sprintf
      "convert %s %s -> ~%s %s at %s"
      (a sell)
      (l from)
      (a buy_estimate)
      (l to_)
      (Rate.price_to_string (Rate.price rate))
  | Subscribe { from; fund; amount } ->
    sprintf "subscribe %s %s -> %s" (a amount) (l from) (l fund)
  | Redeem { fund; to_; amount } ->
    sprintf "redeem %s %s -> %s" (a amount) (l fund) (l to_)
  | Hedge { position; side; size; price } ->
    sprintf
      "hedge %s %s %s at %s"
      (match side with
       | Buy -> "buy"
       | Sell -> "sell")
      (a size)
      (l position)
      (Rate.price_to_string (Rate.price price))
;;
