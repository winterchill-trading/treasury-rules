(** A treasury rule: what it watches, what it proposes, the guards around it, how often it
    runs and in which mode.

    Amounts in a rule are in the currency of the balance they describe (a sweep's
    threshold is in the swept asset). Amounts compared across assets (a rebalance's
    minimum, a hedge's minimum notional, daily caps and approval thresholds) are in the
    settings' valuation currency; a mismatch is reported when the rule is evaluated
    ({!Reason_code.Rule_error}), since the rule does not know the settings. *)

open! Core
open Money

module Target : sig
  (** One leg of a target allocation. *)
  type t =
    { location : Location.t
    ; asset : Currency.t
    ; weight_bps : int (** share of the total value, in basis points *)
    }
  [@@deriving sexp, compare, equal]
end

module Kind : sig
  type t =
    | Sweep of
        { from : Location.t
        ; to_ : Location.t
        ; threshold : Amount.t (** keep this much at [from] *)
        ; minimum : Amount.t
        } (** move everything above [threshold] from [from] to [to_] *)
    | Top_up of
        { from : Location.t
        ; to_ : Location.t
        ; floor : Amount.t (** act when [to_] holds less than this *)
        ; target : Amount.t (** refill [to_] up to this *)
        ; minimum : Amount.t
        } (** keep a balance (a card float, a payroll runway) at or above a floor *)
    | Convert of
        { from : Location.t
        ; to_ : Location.t
        ; threshold : Amount.t (** keep this much of the sold asset *)
        ; buy : Currency.t
        ; minimum : Amount.t (** in the sold asset *)
        ; max_per_run : Amount.t option (** in the sold asset *)
        } (** convert everything above [threshold] into [buy] *)
    | Rebalance of
        { targets : Target.t list
        ; band_bps : int (** tolerated drift of any leg, in basis points of the total *)
        ; minimum : Amount.t (** smallest move, in the valuation currency *)
        }
    (** move value from overweight legs to underweight ones when any leg drifts outside
        the band; legs are brought back to their exact targets *)
    | Yield of
        { cash : Location.t
        ; fund : Location.t
        ; buffer : Amount.t (** cash kept available *)
        ; max_share_bps : int (** the fund's maximum share of cash + fund *)
        ; minimum : Amount.t
        }
    (** place cash above the buffer in a yield or staking position, up to a share of the
        total, and redeem when cash falls below the buffer or the share is exceeded; the
        fund balance is counted in the cash asset (at its net asset value) *)
    | Hedge of
        { asset : Currency.t (** the hedged asset, held at [staked] and [position] *)
        ; staked : Location.t (** the long exposure, e.g. staked tokens *)
        ; position : Location.t (** the hedge; negative is short *)
        ; ratio_bps : int (** target short as a share of [staked] *)
        ; band_bps : int (** tolerated drift, in basis points of the target *)
        ; max_position : Amount.t option (** largest short allowed, in [asset] *)
        ; min_notional : Amount.t (** smallest order, in the valuation currency *)
        } (** keep a short position at a ratio of a staked or held asset *)
  [@@deriving sexp, compare, equal]

  (** ["sweep"], ["top_up"], ["convert"], ["rebalance"], ["yield"], ["hedge"]. *)
  val name : t -> string

  (** Every location the rule reads, in the order given. *)
  val locations : t -> Location.t list
end

module Drawdown : sig
  (** Pause the rule while [asset] trades more than [max_drawdown_bps] below its highest
      price over [lookback] (prices in the valuation currency). *)
  type t =
    { asset : Currency.t
    ; max_drawdown_bps : int
    ; lookback : Time_ns.Span.t
    }
  [@@deriving sexp, compare, equal]
end

module Guards : sig
  type t =
    { daily_cap : Amount.t option
    (** most value this rule may move per UTC day, across its runs *)
    ; approval_above : Amount.t option (** actions above this value need approval *)
    ; drawdown : Drawdown.t option
    }
  [@@deriving sexp, compare, equal]

  val none : t
end

module Status : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Active
        | Paused
        | Archived
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Active
    | Paused
    | Archived
  [@@deriving sexp, compare, equal, enumerate]
end

type t = private
  { id : Rule_id.t
  ; name : string
  ; kind : Kind.t
  ; guards : Guards.t
  ; cadence : Cadence.t
  ; mode : Mode.t
  ; status : Status.t
  ; revision : int (** 1 on creation, +1 on every change *)
  }
[@@deriving sexp_of, compare, equal]

(** A new [Active] rule. [guards] defaults to {!Guards.none}, [cadence] to daily and
    [mode] to [Dry_run], so a new rule proposes nothing until it is promoted. Fails when
    the rule is inconsistent (see {!validate}). *)
val create
  :  id:Rule_id.t
  -> name:string
  -> kind:Kind.t
  -> ?guards:Guards.t
  -> ?cadence:Cadence.t
  -> ?mode:Mode.t
  -> unit
  -> t Or_error.t

(** Checks the name (1 to 80 characters), currencies, signs, basis-point ranges, the
    weights of a rebalance (each in [0, 10000], distinct legs, summing to 10000) and the
    guards. *)
val validate : t -> unit Or_error.t

(** Changes any of the given fields and increments [revision]. Fails on an archived rule
    or an inconsistent result. *)
val update
  :  t
  -> ?name:string
  -> ?kind:Kind.t
  -> ?guards:Guards.t
  -> ?cadence:Cadence.t
  -> ?mode:Mode.t
  -> unit
  -> t Or_error.t

(** [Active] to [Paused]. *)
val pause : t -> t Or_error.t

(** [Paused] to [Active]. *)
val resume : t -> t Or_error.t

(** Any status but [Archived] to [Archived], which is final. *)
val archive : t -> t Or_error.t

(** [Active] with a scheduled mode ({!Mode.is_scheduled}). *)
val is_scheduled : t -> bool

(** Reading validates like {!create}. *)
include Sexpable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
