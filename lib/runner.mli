(** Runs rules against a store: claims each rule's run for the current period, fills
    the snapshot's recent runs from the store, evaluates, records the outcome and audits
    every step. Also manages rules and moves runs through approval and execution.

    The runner never executes actions. A service reads runs in
    [Awaiting_execution], carries out their actions with {!Run_key.action_key} as the
    idempotency key, and reports back with {!Make.executed} or {!Make.failed}.

    Daily caps across rules hold when one runner at a time ticks an organisation (the
    runner evaluates rules one by one and re-reads the runs before each); two runners
    ticking the same organisation at once can each see the other's cap as unused.
    Exactly-once per rule and period holds regardless, through {!Store.S.claim_run}. *)

open! Core

module Result : sig
  type t =
    | Recorded of Run_record.t (** this call evaluated and recorded the run *)
    | Already of Run_record.t (** another call holds or settled the run *)
    | Not_scheduled of Rule_id.t (** paused, archived or in [Dry_run] mode *)
  [@@deriving sexp_of]
end

module Make (M : Monad.S) (Store : Store.S with type 'a io := 'a M.t) : sig
  (** Stores a new rule and audits [Rule_created]. Fails when the id exists. *)
  val create_rule
    :  Store.t
    -> actor:Audit.Actor.t
    -> now:Time_ns.t
    -> Rule.t
    -> unit Or_error.t M.t

  (** Applies [f] to the stored rule ({!Rule.update}, {!Rule.pause}, {!Rule.resume},
      {!Rule.archive}) and saves it if nobody changed it in between. Audits
      [Rule_status_changed] when the status changed, [Rule_updated] otherwise. *)
  val change_rule
    :  Store.t
    -> actor:Audit.Actor.t
    -> now:Time_ns.t
    -> Rule_id.t
    -> f:(Rule.t -> Rule.t Or_error.t)
    -> Rule.t Or_error.t M.t

  (** Runs one rule for the period containing [now]. [snapshot]'s [runs] are replaced
      by the store's runs since the start of the UTC day plus pending ones. *)
  val run_rule
    :  Store.t
    -> Settings.t
    -> Snapshot.t
    -> now:Time_ns.t
    -> actor:Audit.Actor.t
    -> Rule.t
    -> Result.t M.t

  (** {!run_rule} for every stored rule, in id order. *)
  val tick
    :  Store.t
    -> Settings.t
    -> Snapshot.t
    -> now:Time_ns.t
    -> actor:Audit.Actor.t
    -> Result.t list M.t

  (** A dry run: evaluates [rule] (stored or not, any mode or status) with the store's
      recent runs and records nothing. *)
  val preview
    :  Store.t
    -> Settings.t
    -> Snapshot.t
    -> now:Time_ns.t
    -> Rule.t
    -> Outcome.t M.t

  val approve
    :  Store.t
    -> actor:Audit.Actor.t
    -> now:Time_ns.t
    -> Run_key.t
    -> Run_record.t Or_error.t M.t

  val reject
    :  Store.t
    -> actor:Audit.Actor.t
    -> now:Time_ns.t
    -> note:string
    -> Run_key.t
    -> Run_record.t Or_error.t M.t

  val executed
    :  Store.t
    -> actor:Audit.Actor.t
    -> now:Time_ns.t
    -> Run_key.t
    -> Run_record.t Or_error.t M.t

  val failed
    :  Store.t
    -> actor:Audit.Actor.t
    -> now:Time_ns.t
    -> reason:string
    -> Run_key.t
    -> Run_record.t Or_error.t M.t
end
