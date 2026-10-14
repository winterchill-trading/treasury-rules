(** What a rule sees: balances per location and asset, current prices, a price history for
    drawdown guards, and recent runs for caps and pending checks. A service builds it from
    its ledger, price feeds and {!Store}; the evaluation never reads anything else. *)

open! Core
open Money

module Balance : sig
  type t =
    { location : Location.t
    ; amount : Amount.t (** may be negative: a short position *)
    }
  [@@deriving sexp, compare, equal]
end

type t =
  { at : Time_ns.Alternate_sexp.t (** when the balances were read *)
  ; balances : Balance.t list
  ; prices : Rate.t list (** latest observations; the newest per pair wins *)
  ; price_history : Rate.t list (** older observations, for drawdown guards *)
  ; runs : Run_record.t list
  (** runs of every rule since at least the start of the UTC day, and any older run still
      pending *)
  }
[@@deriving sexp, compare, equal]

val create
  :  at:Time_ns.t
  -> ?balances:Balance.t list
  -> ?prices:Rate.t list
  -> ?price_history:Rate.t list
  -> ?runs:Run_record.t list
  -> unit
  -> t

(** The balance of [currency] at [location]; [None] when the snapshot has no such row.
    Several rows add up. *)
val balance : t -> Location.t -> Currency.t -> Amount.t option

(** A rate from [base] to [quote] observed at or after [now - max_age]: the newest direct
    quote, else the inverse of the newest reverse quote, else a cross through [via] (the
    older of the two legs' times). [None] when [base] equals [quote] or nothing fresh is
    known. *)
val rate
  :  t
  -> base:Currency.t
  -> quote:Currency.t
  -> via:Currency.t
  -> now:Time_ns.t
  -> max_age:Time_ns.Span.t
  -> Rate.t option

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
