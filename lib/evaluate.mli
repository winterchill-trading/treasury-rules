(** Pure evaluation of one rule against one snapshot.

    The order of checks is fixed and every check that stops the run records exactly one
    [Block] hit:

    + the rule is [Active] ({!Reason_code.Rule_inactive});
    + the snapshot is recent ({!Reason_code.Snapshot_stale});
    + no earlier run of the rule is still pending ({!Reason_code.Previous_pending});
    + the drawdown guard, when set ({!Reason_code.Drawdown_pause});
    + the rule kind proposes actions (or says why not);
    + the rule's daily cap, then the organisation's daily cap, which may reduce or drop
      actions ({!Reason_code.Daily_cap_applied}) or stop the run when used up;
    + routing: [Review] hits when automatic execution is off or an action's value is
      above the rule's approval threshold.

    Caps count the value of today's (UTC) runs in the snapshot that
    {!Run_record.counts_against_caps}; a [Shadow] rule's own cap counts its own shadowed
    runs instead, so shadow results show what the live rule would have done. A rule that
    raises becomes a {!Reason_code.Rule_error} skip. *)

open! Core

(** [mode] overrides the rule's mode in the outcome, for a dry-run preview of a live
    rule. *)
val evaluate
  :  ?mode:Mode.t
  -> Settings.t
  -> Rule.t
  -> Snapshot.t
  -> now:Time_ns.t
  -> Outcome.t

(** The value of today's (UTC) actions that count against caps, per rule. Used by
    services to show how much of a cap is left. *)
val used_today
  :  Settings.t
  -> Snapshot.t
  -> now:Time_ns.t
  -> rule:Rule_id.t option
  -> Money.Amount.t
