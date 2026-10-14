open! Core

module Actor = struct
  module Stable = struct
    module V1 = struct
      type t =
        | System
        | User of string
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | System
    | User of string
  [@@deriving sexp, compare, equal]
end

module Event = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Rule_created of Rule.Stable.V1.t
        | Rule_updated of Rule.Stable.V1.t
        | Rule_status_changed of
            { from : Rule.Status.Stable.V1.t
            ; to_ : Rule.Status.Stable.V1.t
            ; revision : int
            }
        | Run_claimed of
            { key : Run_key.Stable.V1.t
            ; attempt : int
            }
        | Run_recorded of
            { key : Run_key.Stable.V1.t
            ; status : Run_record.Status.Stable.V1.t
            ; codes : Reason_code.Stable.V1.t list
            }
        | Run_approved of Run_key.Stable.V1.t
        | Run_rejected of
            { key : Run_key.Stable.V1.t
            ; note : string
            }
        | Run_executed of Run_key.Stable.V1.t
        | Run_failed of
            { key : Run_key.Stable.V1.t
            ; reason : string
            }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Rule_created of Rule.t
    | Rule_updated of Rule.t
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
        ; codes : Reason_code.t list
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

module Stable = struct
  module V1 = struct
    type t =
      { at : Time_ns.Stable.Alternate_sexp.V1.t
      ; actor : Actor.Stable.V1.t
      ; rule : Rule_id.Stable.V1.t
      ; event : Event.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { at : Time_ns.Alternate_sexp.t
  ; actor : Actor.t
  ; rule : Rule_id.t
  ; event : Event.t
  }
[@@deriving sexp_of, compare, equal]

let to_string_hum t =
  let actor =
    match t.actor with
    | System -> "system"
    | User u -> "user " ^ u
  in
  let key = Run_key.to_string in
  let status s = Sexp.to_string (Run_record.Status.sexp_of_t s) in
  let what =
    match t.event with
    | Rule_created r ->
      sprintf "created (%s, %s)" (Rule.Kind.name r.kind) (Mode.to_string r.mode)
    | Rule_updated r -> sprintf "updated to r%d (%s)" r.revision (Mode.to_string r.mode)
    | Rule_status_changed { from; to_; revision } ->
      sprintf
        "%s -> %s (r%d)"
        (Sexp.to_string (Rule.Status.sexp_of_t from))
        (Sexp.to_string (Rule.Status.sexp_of_t to_))
        revision
    | Run_claimed { key = k; attempt } ->
      sprintf "claimed %s (attempt %d)" (key k) attempt
    | Run_recorded { key = k; status = s; codes } ->
      sprintf
        "recorded %s: %s [%s]"
        (key k)
        (status s)
        (String.concat ~sep:" " (List.map codes ~f:Reason_code.to_string))
    | Run_approved k -> "approved " ^ key k
    | Run_rejected { key = k; note } -> sprintf "rejected %s: %s" (key k) note
    | Run_executed k -> "executed " ^ key k
    | Run_failed { key = k; reason } -> sprintf "failed %s: %s" (key k) reason
  in
  sprintf
    "%s %s %s: %s"
    (Time_ns.to_string_iso8601_basic t.at ~zone:Time_float.Zone.utc)
    actor
    (Rule_id.to_string t.rule)
    what
;;
