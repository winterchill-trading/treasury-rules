(** The result of evaluating one rule against one snapshot: the proposed actions and every
    finding, with the route derived from them.

    The route is the worst finding, as in risk-rules' [Decision]: any [Block] hit, or no
    action at all, means [Skip]; otherwise any [Review] hit means [Approve]; otherwise
    [Execute]. The record is private so the route can never disagree with the hits. *)

open! Core

module Route : sig
  type t =
    | Execute (** actions may run without a person *)
    | Approve (** actions wait for an approver *)
    | Skip (** nothing to do; the [Block] hits say why *)
  [@@deriving sexp, compare, equal, enumerate]
end

type t = private
  { rule : Rule_id.t
  ; revision : int (** the rule revision that was evaluated *)
  ; mode : Mode.t
  ; evaluated_at : Time_ns.Alternate_sexp.t
  ; route : Route.t
  ; actions : Action.t list (** empty when skipped *)
  ; hits : Hit.t list
  }
[@@deriving sexp_of, compare, equal]

(** Derives the route; drops the actions when it is [Skip]. *)
val create
  :  rule:Rule_id.t
  -> revision:int
  -> mode:Mode.t
  -> evaluated_at:Time_ns.t
  -> actions:Action.t list
  -> hits:Hit.t list
  -> t

(** The worst hit severity ([Allow] when there are none). *)
val severity : t -> Risk_rules.Severity.t

(** The reason codes of the [Block] hits, in order (empty unless skipped). *)
val skip_reasons : t -> Reason_code.t list

(** The outcome as a risk-rules decision for [rule], for shared audit and review tooling.
    A skipped run with no hits gets none. *)
val to_decision : t -> rule:string -> Risk_rules.Decision.t

(** Several lines for logs and operator screens. *)
val to_string_hum : t -> string

(** Reading re-derives the route like {!create}. *)
include Sexpable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
