open! Core

module Stable = struct
  module V1 = struct
    type t =
      { code : Reason_code.Stable.V1.t
      ; severity : Risk_rules.Severity.Stable.V1.t
      ; detail : string
      ; evidence : (string * string) list [@sexp.list]
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { code : Reason_code.t
  ; severity : Risk_rules.Severity.t
  ; detail : string
  ; evidence : (string * string) list [@sexp.list]
  }
[@@deriving sexp, compare, equal]

let make severity code detail evidence = { code; severity; detail; evidence }
let action = make Allow
let review = make Review
let block = make Block

let to_risk_rules t ~rule : Risk_rules.Hit.t =
  { rule
  ; severity = t.severity
  ; code = Reason_code.to_risk_rules t.code
  ; typology =
      (match t.code with
       | Rule_error -> Operational
       | _ -> Limits)
  ; reason = t.detail
  ; evidence = t.evidence
  }
;;
