(** The append-only audit trail: who changed a rule or a run, when, and what happened.
    Entries are values; a {!Store} appends them and never edits one. *)

open! Core

module Actor : sig
  type t =
    | System (** the scheduler *)
    | User of string (** an opaque user id chosen by the service *)
  [@@deriving sexp, compare, equal]
end

module Event : sig
  type t =
    | Rule_created of Rule.t
    | Rule_updated of Rule.t (** the new version; its revision says which *)
    | Rule_status_changed of
        { from : Rule.Status.t
        ; to_ : Rule.Status.t
        ; revision : int
        }
    | Run_claimed of
        { key : Run_key.t
        ; attempt : int
        }
    | Run_recorded of
        { key : Run_key.t
        ; status : Run_record.Status.t
        ; codes : Reason_code.t list (** every hit's code, in order *)
        }
    | Run_approved of Run_key.t
    | Run_rejected of
        { key : Run_key.t
        ; note : string
        }
    | Run_executed of Run_key.t
    | Run_failed of
        { key : Run_key.t
        ; reason : string
        }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { at : Time_ns.Alternate_sexp.t
  ; actor : Actor.t
  ; rule : Rule_id.t
  ; event : Event.t
  }
[@@deriving sexp_of, compare, equal]

(** One line for logs. *)
val to_string_hum : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
