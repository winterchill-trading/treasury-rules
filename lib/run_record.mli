(** The durable record of one run, one per {!Run_key}, and its state machine.

    {v
      Evaluating --record--> Skipped | Shadowed | Awaiting_approval | Awaiting_execution
      Evaluating --fail----> Failed
      Awaiting_approval --approve--> Awaiting_execution
      Awaiting_approval --reject---> Rejected
      Awaiting_execution --executed--> Executed
      Awaiting_execution --fail------> Failed
    v}

    A run is claimed ([Evaluating]) before the rule is evaluated. Evaluation is pure and
    nothing executes while a run is [Evaluating], so a claim left behind by a crashed
    runner can be taken over ({!take_over}) and evaluated again under the same key: the
    action keys ({!Run_key.action_key}) stay the same, and an action is never handed to
    execution twice. Every change increments [version], which stores use for
    compare-and-set. *)

open! Core

module Status : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Evaluating
        | Skipped
        | Shadowed
        | Awaiting_approval
        | Awaiting_execution
        | Executed
        | Failed
        | Rejected
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Evaluating
    | Skipped (** nothing to do; the outcome's hits say why *)
    | Shadowed (** a shadow-mode run; its actions are only recorded *)
    | Awaiting_approval
    | Awaiting_execution (** approved, or routed to execution directly *)
    | Executed
    | Failed
    | Rejected (** an approver refused the actions *)
  [@@deriving sexp, compare, equal, enumerate]

  (** No transition leaves it: [Skipped], [Shadowed], [Executed], [Failed], [Rejected]. *)
  val is_final : t -> bool
end

type t = private
  { key : Run_key.t
  ; mode : Mode.t
  ; status : Status.t
  ; attempt : int (** claims taken on the key; 2 or more after a take-over *)
  ; outcome : Outcome.t option (** set when leaving [Evaluating] by {!record} *)
  ; note : string option (** a failure reason or a rejection note *)
  ; version : int (** 1 on claim, +1 on every change *)
  ; created_at : Time_ns.Alternate_sexp.t
  ; updated_at : Time_ns.Alternate_sexp.t
  }
[@@deriving sexp_of, compare, equal]

(** A fresh claim: [Evaluating], attempt 1, version 1. *)
val claim : Run_key.t -> mode:Mode.t -> now:Time_ns.t -> t

(** [Evaluating] and last changed before [now - stale_after]. *)
val is_stale : t -> now:Time_ns.t -> stale_after:Time_ns.Span.t -> bool

(** A stale claim taken over: attempt + 1. Fails unless [Evaluating]. *)
val take_over : t -> now:Time_ns.t -> t Or_error.t

(** Settles the evaluation. A [Shadow] run goes to [Shadowed] unless skipped; a [Live] run
    goes to [Skipped], [Awaiting_approval] or [Awaiting_execution] by the outcome's route.
    Fails unless [Evaluating], or when the outcome is for another rule. *)
val record : t -> Outcome.t -> now:Time_ns.t -> t Or_error.t

(** [Awaiting_approval] to [Awaiting_execution]. Who may approve (roles, a second
    approver) is the service's policy. *)
val approve : t -> now:Time_ns.t -> t Or_error.t

(** [Awaiting_approval] to [Rejected], keeping [note]. *)
val reject : t -> note:string -> now:Time_ns.t -> t Or_error.t

(** The executor finished every action. *)
val executed : t -> now:Time_ns.t -> t Or_error.t

(** From [Evaluating] (the rule raised) or [Awaiting_execution] (an action failed). *)
val fail : t -> reason:string -> now:Time_ns.t -> t Or_error.t

(** The proposed actions, empty before {!record} or when skipped. *)
val actions : t -> Action.t list

(** [Awaiting_approval] or [Awaiting_execution]: a later run of the same rule waits. *)
val is_pending : t -> bool

(** Whether the run's actions use up daily caps: a [Live] run that is awaiting approval,
    awaiting execution or executed. *)
val counts_against_caps : t -> bool

(** Reading checks the record's internal consistency (status against outcome, positive
    attempt and version). *)
include Sexpable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
