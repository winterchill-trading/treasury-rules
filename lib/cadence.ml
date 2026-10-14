open! Core

let validate span =
  if Time_ns.Span.( < ) span Time_ns.Span.minute
  then Or_error.error_s [%message "cadence shorter than a minute" (span : Time_ns.Span.t)]
  else if Time_ns.Span.( > ) span (Time_ns.Span.scale_int Time_ns.Span.day 31)
  then Or_error.error_s [%message "cadence longer than 31 days" (span : Time_ns.Span.t)]
  else Ok span
;;

module Stable = struct
  module V1 = struct
    module Repr = struct
      type t = Time_ns.Span.Stable.V2.t [@@deriving bin_io, sexp, compare, equal, hash]
    end

    type t = Time_ns.Span.t [@@deriving compare, equal, hash, sexp_of]

    let of_repr span = Or_error.ok_exn (validate span)
    let t_of_sexp sexp = of_repr (Repr.t_of_sexp sexp)

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type nonrec t = t

          let to_binable = Fn.id
          let of_binable = of_repr
        end)
  end
end

include Stable.V1

let create = validate
let create_exn span = Or_error.ok_exn (validate span)
let hourly = Time_ns.Span.hour
let daily = Time_ns.Span.day
let span = Fn.id

let period_start t now =
  let step = Time_ns.Span.to_int_ns t in
  let ns = Time_ns.to_int_ns_since_epoch now in
  (* Floor division, so instants before the epoch still land on a period start. *)
  let q = if ns >= 0 then ns / step else ((ns + 1) / step) - 1 in
  Time_ns.of_int_ns_since_epoch (q * step)
;;

let next_period_start t now = Time_ns.add (period_start t now) t

let quickcheck_generator =
  Quickcheck.Generator.map (Int.gen_incl 1 (31 * 24 * 60)) ~f:(fun minutes ->
    Time_ns.Span.of_int_sec (minutes * 60))
;;

let quickcheck_observer = Quickcheck.Observer.unmap Int.quickcheck_observer ~f:Time_ns.Span.to_int_ns
let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
