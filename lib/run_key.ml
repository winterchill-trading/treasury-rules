open! Core

module Stable = struct
  module V1 = struct
    type t =
      { rule : Rule_id.Stable.V1.t
      ; period_start : Time_ns.Stable.Alternate_sexp.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

module T = struct
  type t = Stable.V1.t =
    { rule : Rule_id.t
    ; period_start : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal, hash]
end

include T

let create rule cadence ~now = { rule; period_start = Cadence.period_start cadence now }

let to_string { rule; period_start } =
  let at =
    Time_ns.to_string_iso8601_basic period_start ~zone:Time_float.Zone.utc
    |> String.chop_suffix_if_exists ~suffix:".000000000Z"
  in
  Rule_id.to_string rule ^ ":" ^ at ^ if String.is_suffix at ~suffix:"Z" then "" else "Z"
;;

let action_key t ~index = to_string t ^ ":" ^ Int.to_string index

include Comparable.Make (T)
include Hashable.Make (T)
