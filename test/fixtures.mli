(** Shared values for the tests: currencies, locations, a clock, snapshots and rules. *)

open! Core
open Money
open Treasury_rules

(** HYPE with 8 digits, registered on first use. *)
val hype : Currency.t

val eur : Currency.t
val usdc : Currency.t
val a : string -> Amount.t

(** [loc "bank:main"]. *)
val loc : string -> Location.t

val id : string -> Rule_id.t

(** 2026-10-14 10:00:00 UTC. *)
val now : Time_ns.t

val at : string -> Time_ns.t

(** USDC valuation, defaults otherwise. *)
val settings : Settings.t

(** [price "HYPE" "USDC" "25"] observed at [now]. *)
val price : ?at:Time_ns.t -> string -> string -> string -> Rate.t

val bal : string -> string -> Snapshot.Balance.t

val snapshot
  :  ?at:Time_ns.t
  -> ?prices:Rate.t list
  -> ?history:Rate.t list
  -> ?runs:Run_record.t list
  -> Snapshot.Balance.t list
  -> Snapshot.t

(** A [Live], daily rule. *)
val rule
  :  ?guards:Rule.Guards.t
  -> ?mode:Mode.t
  -> ?cadence:Cadence.t
  -> string
  -> Rule.Kind.t
  -> Rule.t

(** Evaluates and prints {!Outcome.to_string_hum}, then the evidence of every hit. *)
val show : ?settings:Settings.t -> ?now:Time_ns.t -> Rule.t -> Snapshot.t -> unit
