open! Core

module Route = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Execute
        | Approve
        | Skip
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Execute
    | Approve
    | Skip
  [@@deriving sexp, compare, equal, enumerate]
end

module Repr = struct
  type t =
    { rule : Rule_id.Stable.V1.t
    ; revision : int
    ; mode : Mode.Stable.V1.t
    ; evaluated_at : Time_ns.Stable.Alternate_sexp.V1.t
    ; route : Route.Stable.V1.t
    ; actions : Action.Stable.V1.t list
    ; hits : Hit.Stable.V1.t list
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { rule : Rule_id.t
  ; revision : int
  ; mode : Mode.t
  ; evaluated_at : Time_ns.Alternate_sexp.t
  ; route : Route.t
  ; actions : Action.t list
  ; hits : Hit.t list
  }
[@@deriving sexp_of, compare, equal]

let worst hits =
  List.fold hits ~init:Risk_rules.Severity.Allow ~f:(fun acc (h : Hit.t) ->
    Risk_rules.Severity.worst acc h.severity)
;;

let create ~rule ~revision ~mode ~evaluated_at ~actions ~hits =
  let route : Route.t =
    match worst hits, actions with
    | Block, _ | _, [] -> Skip
    | Review, _ -> Approve
    | (Allow | Challenge), _ -> Execute
  in
  let actions =
    match route with
    | Skip -> []
    | Execute | Approve -> actions
  in
  { rule; revision; mode; evaluated_at; route; actions; hits }
;;

let severity t = worst t.hits

let skip_reasons t =
  List.filter_map t.hits ~f:(fun h ->
    match h.severity with
    | Block -> Some h.code
    | Allow | Challenge | Review -> None)
;;

let to_decision t ~rule =
  Risk_rules.Decision.of_hits (List.map t.hits ~f:(Hit.to_risk_rules ~rule))
;;

let to_string_hum t =
  let route =
    match t.route with
    | Execute -> "execute"
    | Approve -> "approve"
    | Skip -> "skip"
  in
  let head =
    sprintf "%s r%d (%s): %s" (Rule_id.to_string t.rule) t.revision (Mode.to_string t.mode)
      route
  in
  let actions = List.map t.actions ~f:(fun a -> "  " ^ Action.to_string_hum a) in
  let hits =
    List.map t.hits ~f:(fun (h : Hit.t) ->
      sprintf
        "  [%s] %s: %s"
        (Risk_rules.Severity.to_string h.severity)
        (Reason_code.to_string h.code)
        h.detail)
  in
  String.concat ~sep:"\n" ((head :: actions) @ hits)
;;

let of_repr (r : Repr.t) =
  create
    ~rule:r.rule
    ~revision:r.revision
    ~mode:r.mode
    ~evaluated_at:r.evaluated_at
    ~actions:r.actions
    ~hits:r.hits
;;

let t_of_sexp sexp = of_repr (Repr.t_of_sexp sexp)

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, sexp_of]

    let t_of_sexp = t_of_sexp

    (* Re-derive the route on read, so a damaged record cannot route a run to execution
       that its hits would skip. *)
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
