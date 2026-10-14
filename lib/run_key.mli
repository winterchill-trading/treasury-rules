(** The identity of one run: a rule and the start of a period. A store keeps at most one
    run record per key, which is what makes runs exactly-once. *)

open! Core

type t =
  { rule : Rule_id.t
  ; period_start : Time_ns.Alternate_sexp.t
  }
[@@deriving sexp, compare, equal, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

val create : Rule_id.t -> Cadence.t -> now:Time_ns.t -> t

(** ["sweep-eur:2026-10-14T00:00:00Z"], usable as an idempotency key. *)
val to_string : t -> string

(** A key for the [index]th action of the run (["...:0"], ["...:1"]): the same on every
    attempt, so an executor can use it as an idempotency key with its rails. *)
val action_key : t -> index:int -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
