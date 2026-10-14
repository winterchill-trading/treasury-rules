(** Scenarios every {!Store.S} implementation must pass: rule insert and
    compare-and-set, run claim, duplicate claim, take-over of a stale claim,
    compare-and-set updates, recent-run listing and audit order. A service runs them
    against its database store in its own test suite, next to {!Store_memory}.

    Give it a fresh, empty store; the scenarios use rule ids starting with
    [conformance-] and times in January 2030. *)

open! Core

module Make (M : Monad.S) (Store : Store.S with type 'a io := 'a M.t) : sig
  (** [Ok ()] when every scenario passes, otherwise the first difference. *)
  val check : Store.t -> unit Or_error.t M.t
end
