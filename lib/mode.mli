(** How a rule's proposals are used.

    - [Live]: scheduled runs are recorded and their actions go to approval or execution.
    - [Shadow]: scheduled runs are recorded with their proposed actions, but nothing is
      executed and nothing counts against daily caps. Use it to watch a new or changed
      rule next to the live ones before trusting it.
    - [Dry_run]: the scheduler skips the rule; it is only evaluated on demand
      ({!Runner.Make.preview}), and nothing is recorded. A draft rule starts here. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Live
      | Shadow
      | Dry_run
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

type t = Stable.V1.t =
  | Live
  | Shadow
  | Dry_run
[@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

(** ["live"], ["shadow"], ["dry_run"]. *)
val to_string : t -> string

(** Whether the scheduler runs and records the rule ([Live] and [Shadow]). *)
val is_scheduled : t -> bool
