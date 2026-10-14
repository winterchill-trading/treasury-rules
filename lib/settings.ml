open! Core
open Money

module Stable = struct
  module V1 = struct
    type t =
      { valuation : Currency.Stable.V1.t
      ; org_daily_cap : Amount.Stable.V1.t option [@sexp.option]
      ; auto_execute : bool
      ; max_price_age : Time_ns.Span.Stable.V2.t
      ; max_snapshot_age : Time_ns.Span.Stable.V2.t
      ; stale_claim_after : Time_ns.Span.Stable.V2.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { valuation : Currency.t
  ; org_daily_cap : Amount.t option [@sexp.option]
  ; auto_execute : bool
  ; max_price_age : Time_ns.Span.t
  ; max_snapshot_age : Time_ns.Span.t
  ; stale_claim_after : Time_ns.Span.t
  }
[@@deriving sexp, compare, equal]

let default ~valuation =
  { valuation
  ; org_daily_cap = None
  ; auto_execute = true
  ; max_price_age = Time_ns.Span.of_min 15.
  ; max_snapshot_age = Time_ns.Span.of_min 15.
  ; stale_claim_after = Time_ns.Span.of_min 2.
  }
;;

let validate t =
  let span name s =
    if Time_ns.Span.is_positive s
    then Ok ()
    else Or_error.error_s [%message "settings: span must be positive" name]
  in
  Or_error.combine_errors_unit
    [ span "max_price_age" t.max_price_age
    ; span "max_snapshot_age" t.max_snapshot_age
    ; span "stale_claim_after" t.stale_claim_after
    ; (match t.org_daily_cap with
       | None -> Ok ()
       | Some cap ->
         if Amount.is_positive cap && Currency.equal (Amount.currency cap) t.valuation
         then Ok ()
         else
           Or_error.error_s
             [%message
               "settings: org_daily_cap must be positive and in the valuation currency"
                 (cap : Amount.t)])
    ]
;;
