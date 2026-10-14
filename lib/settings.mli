(** Organisation-wide settings that every rule evaluation reads. *)

open! Core
open Money

type t =
  { valuation : Currency.t
  (** the currency values, daily caps and approval thresholds are expressed in *)
  ; org_daily_cap : Amount.t option
  (** most value all live rules together may move per UTC day *)
  ; auto_execute : bool
  (** [false] sends every live proposal to approval
      ({!Reason_code.Manual_execution}) *)
  ; max_price_age : Time_ns.Span.t (** older prices count as unavailable *)
  ; max_snapshot_age : Time_ns.Span.t (** older balances make every rule skip *)
  ; stale_claim_after : Time_ns.Span.t
  (** a run still evaluating after this long belongs to a crashed runner and is taken
      over *)
  }
[@@deriving sexp, compare, equal]

(** No organisation cap, automatic execution, prices and balances valid for 15 minutes,
    claims stale after 2 minutes. *)
val default : valuation:Currency.t -> t

(** Fails on a non-positive span, or an organisation cap that is not positive or not in
    the valuation currency. *)
val validate : t -> unit Or_error.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
