(** Stable reason codes for treasury rule outcomes.

    Every proposed action, every skipped run and every routing decision carries one. The
    text form (["SWEEP_EXCESS"], ["DRAWDOWN_PAUSE"], ["CUSTOM_MY_CHECK"]) is the stable
    identifier written to audit trails and shown to operators; sexp uses it too.

    Codes fall in three groups ({!group}): why an action is proposed, why a run proposes
    nothing, and why proposed actions need a person or were reduced. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Sweep_excess (** a balance above its threshold is moved on *)
      | Top_up_floor (** a balance below its floor is refilled *)
      | Convert_excess (** a balance above its threshold is converted *)
      | Rebalance_drift (** an allocation drifted outside its band *)
      | Yield_subscribe (** idle cash above the buffer goes into yield *)
      | Yield_redeem_liquidity (** cash fell below the buffer; yield is redeemed *)
      | Yield_redeem_over_limit (** yield exceeds its allocation limit *)
      | Hedge_adjust (** a hedge drifted from its target ratio *)
      | Rule_inactive (** the rule is paused or archived *)
      | Previous_pending (** an earlier run of the rule has unfinished actions *)
      | No_balance (** a location the rule needs is missing from the snapshot *)
      | Snapshot_stale (** balances are older than the settings allow *)
      | Price_unavailable (** a needed price is missing or too old *)
      | Below_threshold (** nothing above the threshold to move *)
      | Below_minimum (** the move would be smaller than the rule's minimum *)
      | Floor_met (** the balance is already at or above its floor *)
      | Below_buffer (** no cash above the buffer to place *)
      | Within_band (** allocations or hedge are within their band *)
      | Allocation_limit_reached (** yield is already at its limit *)
      | Insufficient_balance (** the source cannot fund the move *)
      | Position_limit_reached (** the hedge is at its maximum size *)
      | Rule_daily_cap_reached (** the rule moved its daily cap already *)
      | Org_daily_cap_reached (** all rules together moved the organisation's daily cap *)
      | Drawdown_pause (** the reference price fell more than allowed *)
      | Daily_cap_applied (** an action was reduced to fit a daily cap *)
      | Approval_required (** an action is above the approval threshold *)
      | Manual_execution (** the settings route every proposal to approval *)
      | Rule_error (** the rule could not be evaluated *)
      | Custom of string (** codes of rules written outside this library *)
    [@@deriving bin_io, compare, equal, hash]

    include Sexpable.S with type t := t
  end
end

type t = Stable.V1.t =
  | Sweep_excess
  | Top_up_floor
  | Convert_excess
  | Rebalance_drift
  | Yield_subscribe
  | Yield_redeem_liquidity
  | Yield_redeem_over_limit
  | Hedge_adjust
  | Rule_inactive
  | Previous_pending
  | No_balance
  | Snapshot_stale
  | Price_unavailable
  | Below_threshold
  | Below_minimum
  | Floor_met
  | Below_buffer
  | Within_band
  | Allocation_limit_reached
  | Insufficient_balance
  | Position_limit_reached
  | Rule_daily_cap_reached
  | Org_daily_cap_reached
  | Drawdown_pause
  | Daily_cap_applied
  | Approval_required
  | Manual_execution
  | Rule_error
  | Custom of string
[@@deriving compare, equal, hash, quickcheck]

include Sexpable.S with type t := t
include Stringable.S with type t := t
include Comparable.S with type t := t

module Group : sig
  type t =
    | Action (** explains a proposed action *)
    | Skip (** explains why a run proposes nothing *)
    | Route (** explains an approval route or a reduced amount *)
  [@@deriving sexp, compare, equal, enumerate]
end

(** [Custom] codes count as [Skip]. *)
val group : t -> Group.t

(** Every code except [Custom]. *)
val builtin : t list

(** One line for operators: what the code means. *)
val description : t -> string

(** The same code as a risk-rules code ([Custom "TREASURY_<code>"]), so treasury outcomes
    can share audit and review tooling with risk decisions. *)
val to_risk_rules : t -> Risk_rules.Reason_code.t
