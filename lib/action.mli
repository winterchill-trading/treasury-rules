(** A proposed treasury action. The library never executes anything: a service turns
    actions into ledger entries, payment orders, exchange orders or fund subscriptions,
    using {!Run_key.action_key} as the idempotency key. *)

open! Core
open Money

module Side : sig
  type t =
    | Buy
    | Sell
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Kind : sig
  type t =
    | Transfer of
        { from : Location.t
        ; to_ : Location.t
        ; amount : Amount.t
        }
    (** move an amount between two locations of the same asset *)
    | Convert of
        { from : Location.t
        ; to_ : Location.t
        ; sell : Amount.t
        ; buy_estimate : Amount.t
        (** [sell] at [rate], rounded down; the fill decides the real amount *)
        ; rate : Rate.t (** base = sold currency, quote = bought currency *)
        }
    (** sell one asset at [from] and receive another at [to_] *)
    | Subscribe of
        { from : Location.t
        ; fund : Location.t
        ; amount : Amount.t
        }
    (** place cash in a yield or staking position *)
    | Redeem of
        { fund : Location.t
        ; to_ : Location.t
        ; amount : Amount.t
        }
    (** take cash out of a yield or staking position *)
    | Hedge of
        { position : Location.t
        ; side : Side.t
        ; size : Amount.t (** positive, in the hedged asset *)
        ; price : Rate.t (** base = hedged asset, quote = valuation currency *)
        }
    (** change a derivatives position; [Sell] makes it shorter *)
  [@@deriving sexp, compare, equal]
end

type t =
  { kind : Kind.t
  ; reason : Reason_code.t
  ; value : Amount.t
  (** in the settings' valuation currency; what daily caps and approval thresholds
      compare *)
  ; minimum : Amount.t
  (** the smallest {!amount} worth doing, in its currency; a cap that cuts the action
      below it drops the action *)
  }
[@@deriving sexp, compare, equal]

(** The amount the action moves: [amount], [sell] or [size]. *)
val amount : t -> Amount.t

(** [scale t ratio] shrinks the action by [ratio] (between 0 and 1), rounding the amount
    and value down and re-deriving a conversion's estimate. [None] when the result is
    zero or below [minimum]. *)
val scale : t -> Q.t -> t option

(** One line for logs: ["transfer 1500.00 EUR bank:main -> bank:savings"]. *)
val to_string_hum : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
