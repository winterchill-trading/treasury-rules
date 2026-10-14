(** One finding of an evaluation: a reason code, how it routes the run, a detailed reason
    for operators and the audit trail, and the inputs that were compared.

    The shape follows risk-rules' [Hit]: severity [Allow] explains a proposed action,
    [Review] sends the run to approval, [Block] makes the run propose nothing. *)

open! Core

type t =
  { code : Reason_code.t
  ; severity : Risk_rules.Severity.t
  ; detail : string
  ; evidence : (string * string) list (** e.g. [("threshold", "10000.00 EUR")] *)
  }
[@@deriving sexp, compare, equal]

(** [action code detail evidence] explains a proposed action (severity [Allow]). *)
val action : Reason_code.t -> string -> (string * string) list -> t

(** A finding that sends the run to approval (severity [Review]). *)
val review : Reason_code.t -> string -> (string * string) list -> t

(** A finding that makes the run propose nothing (severity [Block]). *)
val block : Reason_code.t -> string -> (string * string) list -> t

(** The same finding as a risk-rules hit for [rule] (typology [Limits], or [Operational]
    for {!Reason_code.Rule_error}). *)
val to_risk_rules : t -> rule:string -> Risk_rules.Hit.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
