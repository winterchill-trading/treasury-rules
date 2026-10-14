open! Core

module Stable = struct
  module V1 = struct
    module T = struct
      type t =
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
      [@@deriving bin_io, compare, equal, hash, quickcheck, sexp]
    end

    include T

    let custom_prefix = "CUSTOM_"

    let to_string = function
      | Custom name -> custom_prefix ^ name
      | code -> String.uppercase (Sexp.to_string (T.sexp_of_t code))
    ;;

    let of_string s =
      match String.chop_prefix s ~prefix:custom_prefix with
      | Some name when not (String.is_empty name) -> Custom name
      | Some _ | None ->
        (match String.capitalize (String.lowercase s) with
         | "Custom" -> raise_s [%message "reason code: Custom needs a name" s]
         | ctor -> T.t_of_sexp (Sexp.Atom ctor))
    ;;

    let sexp_of_t t = Sexp.Atom (to_string t)

    let t_of_sexp = function
      | Sexp.Atom s -> of_string s
      | sexp -> raise_s [%message "reason code: expected an atom" (sexp : Sexp.t)]
    ;;
  end
end

include Stable.V1

let quickcheck_generator = T.quickcheck_generator
let quickcheck_observer = T.quickcheck_observer
let quickcheck_shrinker = T.quickcheck_shrinker

module Group = struct
  type t =
    | Action
    | Skip
    | Route
  [@@deriving sexp, compare, equal, enumerate]
end

let group : t -> Group.t = function
  | Sweep_excess
  | Top_up_floor
  | Convert_excess
  | Rebalance_drift
  | Yield_subscribe
  | Yield_redeem_liquidity
  | Yield_redeem_over_limit
  | Hedge_adjust -> Action
  | Daily_cap_applied | Approval_required | Manual_execution -> Route
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
  | Rule_error
  | Custom _ -> Skip
;;

let builtin =
  [ Sweep_excess
  ; Top_up_floor
  ; Convert_excess
  ; Rebalance_drift
  ; Yield_subscribe
  ; Yield_redeem_liquidity
  ; Yield_redeem_over_limit
  ; Hedge_adjust
  ; Rule_inactive
  ; Previous_pending
  ; No_balance
  ; Snapshot_stale
  ; Price_unavailable
  ; Below_threshold
  ; Below_minimum
  ; Floor_met
  ; Below_buffer
  ; Within_band
  ; Allocation_limit_reached
  ; Insufficient_balance
  ; Position_limit_reached
  ; Rule_daily_cap_reached
  ; Org_daily_cap_reached
  ; Drawdown_pause
  ; Daily_cap_applied
  ; Approval_required
  ; Manual_execution
  ; Rule_error
  ]
;;

let description = function
  | Sweep_excess -> "balance above the threshold is moved to the destination"
  | Top_up_floor -> "balance below the floor is refilled from the source"
  | Convert_excess -> "balance above the threshold is converted into another asset"
  | Rebalance_drift -> "an allocation drifted outside its band and is moved back"
  | Yield_subscribe -> "cash above the buffer is placed in yield"
  | Yield_redeem_liquidity -> "cash fell below the buffer; yield is redeemed to refill it"
  | Yield_redeem_over_limit -> "yield is above its allocation limit and is reduced"
  | Hedge_adjust -> "the hedge drifted from its target ratio and is adjusted"
  | Rule_inactive -> "the rule is paused or archived"
  | Previous_pending -> "an earlier run of this rule still has unfinished actions"
  | No_balance -> "a location the rule needs is missing from the snapshot"
  | Snapshot_stale -> "balances are older than allowed"
  | Price_unavailable -> "a needed price is missing or too old"
  | Below_threshold -> "nothing above the threshold"
  | Below_minimum -> "the move would be smaller than the rule's minimum"
  | Floor_met -> "the balance is at or above its floor"
  | Below_buffer -> "no cash above the buffer"
  | Within_band -> "within the band; nothing to do"
  | Allocation_limit_reached -> "yield is at its allocation limit"
  | Insufficient_balance -> "the source cannot fund the move"
  | Position_limit_reached -> "the hedge is at its maximum position"
  | Rule_daily_cap_reached -> "the rule's daily cap is used up"
  | Org_daily_cap_reached -> "the organisation's daily cap across rules is used up"
  | Drawdown_pause -> "the reference price fell more than the rule allows"
  | Daily_cap_applied -> "an action was reduced to fit a daily cap"
  | Approval_required -> "an action is above the approval threshold"
  | Manual_execution -> "automatic execution is off; every proposal needs approval"
  | Rule_error -> "the rule could not be evaluated"
  | Custom name -> "custom check " ^ name
;;

let to_risk_rules t : Risk_rules.Reason_code.t = Custom ("TREASURY_" ^ to_string t)

include Comparable.Make (struct
    type nonrec t = t [@@deriving sexp, compare]
  end)
