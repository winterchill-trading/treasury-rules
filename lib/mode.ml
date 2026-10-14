open! Core

module Stable = struct
  module V1 = struct
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

let to_string = function
  | Live -> "live"
  | Shadow -> "shadow"
  | Dry_run -> "dry_run"
;;

let is_scheduled = function
  | Live | Shadow -> true
  | Dry_run -> false
;;
