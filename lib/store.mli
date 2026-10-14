(** Where rules, run records and the audit trail live. Libraries never persist: a
    service implements this signature over its database, and {!Store_memory} serves
    tests, previews and offline modes. {!Store_conformance} checks an implementation.

    Exactly-once rests on {!S.claim_run}: it must be atomic per run key (an insert that
    fails on a duplicate key, or a compare-and-set on the record's version), so of two
    runners racing for one rule and period only one gets [Claimed]. Every later change
    goes through {!S.update_run}, a compare-and-set on [version]; the loser of a race
    gets [`Conflict] with the current record and must not act on its own copy. *)

open! Core

module Claim : sig
  type t =
    | Claimed of Run_record.t
    (** the caller owns the run now: a fresh claim (attempt 1) or a stale one taken over
        (attempt 2 or more), already stored *)
    | Running of Run_record.t (** another runner holds a fresh claim *)
    | Settled of Run_record.t (** the run left [Evaluating] already *)
  [@@deriving sexp_of]
end

module type S = sig
  type 'a io
  type t

  val find_rule : t -> Rule_id.t -> Rule.t option io

  (** Every rule, archived ones included, by id. *)
  val rules : t -> Rule.t list io

  (** Inserts when [expected_revision] is [None] and no rule has the id; replaces when
      the stored rule has [expected_revision]. Otherwise [`Conflict] with what is
      stored. *)
  val save_rule
    :  t
    -> expected_revision:int option
    -> Rule.t
    -> [ `Saved | `Conflict of Rule.t option ] io

  (** Takes the run: stores {!Run_record.claim} when the key is new, replaces an
      [Evaluating] record older than [stale_after] by {!Run_record.take_over}, and
      otherwise returns the stored record. *)
  val claim_run
    :  t
    -> Run_key.t
    -> mode:Mode.t
    -> now:Time_ns.t
    -> stale_after:Time_ns.Span.t
    -> Claim.t io

  (** Replaces the record with the same key when its stored version is
      [expected_version]. *)
  val update_run
    :  t
    -> expected_version:int
    -> Run_record.t
    -> [ `Updated | `Conflict of Run_record.t option ] io

  val find_run : t -> Run_key.t -> Run_record.t option io

  (** Runs whose period starts at or after [since], plus every pending run
      ({!Run_record.is_pending}) whatever its period; by key. *)
  val recent_runs : t -> since:Time_ns.t -> Run_record.t list io

  (** The rule's runs, newest period first, at most [limit]. *)
  val runs_of_rule : t -> Rule_id.t -> limit:int -> Run_record.t list io

  val append_audit : t -> Audit.t -> unit io

  (** In append order; only [rule]'s entries when given. *)
  val audit : t -> rule:Rule_id.t option -> Audit.t list io
end
