(** How often a rule runs: a fixed span, at least one minute, with periods aligned to the
    Unix epoch in UTC. An hourly rule has one period per clock hour, a daily one per UTC
    day. A run is recorded at most once per rule and period, whenever in the period the
    scheduler ticks. *)

open! Core

type t [@@deriving sexp, compare, equal, hash]

(** Fails on a span shorter than one minute or longer than 31 days. *)
val create : Time_ns.Span.t -> t Or_error.t

val create_exn : Time_ns.Span.t -> t
val hourly : t
val daily : t
val span : t -> Time_ns.Span.t

(** The start of the period containing [now]. *)
val period_start : t -> Time_ns.t -> Time_ns.t

(** The start of the period after the one containing [now]. *)
val next_period_start : t -> Time_ns.t -> Time_ns.t

include Quickcheckable.S with type t := t

module Stable : sig
  module V1 : sig
    (** The span (sexp ["1h"], ["1d"]); reading validates like {!create}. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
