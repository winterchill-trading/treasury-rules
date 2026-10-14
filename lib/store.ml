open! Core

module Claim = struct
  type t =
    | Claimed of Run_record.t
    | Running of Run_record.t
    | Settled of Run_record.t
  [@@deriving sexp_of]
end

module type S = sig
  type 'a io
  type t

  val find_rule : t -> Rule_id.t -> Rule.t option io
  val rules : t -> Rule.t list io

  val save_rule
    :  t
    -> expected_revision:int option
    -> Rule.t
    -> [ `Saved | `Conflict of Rule.t option ] io

  val claim_run
    :  t
    -> Run_key.t
    -> mode:Mode.t
    -> now:Time_ns.t
    -> stale_after:Time_ns.Span.t
    -> Claim.t io

  val update_run
    :  t
    -> expected_version:int
    -> Run_record.t
    -> [ `Updated | `Conflict of Run_record.t option ] io

  val find_run : t -> Run_key.t -> Run_record.t option io
  val recent_runs : t -> since:Time_ns.t -> Run_record.t list io
  val runs_of_rule : t -> Rule_id.t -> limit:int -> Run_record.t list io
  val append_audit : t -> Audit.t -> unit io
  val audit : t -> rule:Rule_id.t option -> Audit.t list io
end
