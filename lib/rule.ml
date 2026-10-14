open! Core
open Money

module Target = struct
  module Stable = struct
    module V1 = struct
      type t =
        { location : Location.Stable.V1.t
        ; asset : Currency.Stable.V1.t
        ; weight_bps : int
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    { location : Location.t
    ; asset : Currency.t
    ; weight_bps : int
    }
  [@@deriving sexp, compare, equal]
end

module Kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Sweep of
            { from : Location.Stable.V1.t
            ; to_ : Location.Stable.V1.t
            ; threshold : Amount.Stable.V1.t
            ; minimum : Amount.Stable.V1.t
            }
        | Top_up of
            { from : Location.Stable.V1.t
            ; to_ : Location.Stable.V1.t
            ; floor : Amount.Stable.V1.t
            ; target : Amount.Stable.V1.t
            ; minimum : Amount.Stable.V1.t
            }
        | Convert of
            { from : Location.Stable.V1.t
            ; to_ : Location.Stable.V1.t
            ; threshold : Amount.Stable.V1.t
            ; buy : Currency.Stable.V1.t
            ; minimum : Amount.Stable.V1.t
            ; max_per_run : Amount.Stable.V1.t option [@sexp.option]
            }
        | Rebalance of
            { targets : Target.Stable.V1.t list
            ; band_bps : int
            ; minimum : Amount.Stable.V1.t
            }
        | Yield of
            { cash : Location.Stable.V1.t
            ; fund : Location.Stable.V1.t
            ; buffer : Amount.Stable.V1.t
            ; max_share_bps : int
            ; minimum : Amount.Stable.V1.t
            }
        | Hedge of
            { asset : Currency.Stable.V1.t
            ; staked : Location.Stable.V1.t
            ; position : Location.Stable.V1.t
            ; ratio_bps : int
            ; band_bps : int
            ; max_position : Amount.Stable.V1.t option [@sexp.option]
            ; min_notional : Amount.Stable.V1.t
            }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Sweep of
        { from : Location.t
        ; to_ : Location.t
        ; threshold : Amount.t
        ; minimum : Amount.t
        }
    | Top_up of
        { from : Location.t
        ; to_ : Location.t
        ; floor : Amount.t
        ; target : Amount.t
        ; minimum : Amount.t
        }
    | Convert of
        { from : Location.t
        ; to_ : Location.t
        ; threshold : Amount.t
        ; buy : Currency.t
        ; minimum : Amount.t
        ; max_per_run : Amount.t option [@sexp.option]
        }
    | Rebalance of
        { targets : Target.t list
        ; band_bps : int
        ; minimum : Amount.t
        }
    | Yield of
        { cash : Location.t
        ; fund : Location.t
        ; buffer : Amount.t
        ; max_share_bps : int
        ; minimum : Amount.t
        }
    | Hedge of
        { asset : Currency.t
        ; staked : Location.t
        ; position : Location.t
        ; ratio_bps : int
        ; band_bps : int
        ; max_position : Amount.t option [@sexp.option]
        ; min_notional : Amount.t
        }
  [@@deriving sexp, compare, equal]

  let name = function
    | Sweep _ -> "sweep"
    | Top_up _ -> "top_up"
    | Convert _ -> "convert"
    | Rebalance _ -> "rebalance"
    | Yield _ -> "yield"
    | Hedge _ -> "hedge"
  ;;

  let locations = function
    | Sweep { from; to_; _ } | Top_up { from; to_; _ } | Convert { from; to_; _ } ->
      [ from; to_ ]
    | Rebalance { targets; _ } ->
      List.fold targets ~init:[] ~f:(fun acc (x : Target.t) ->
        if List.mem acc x.location ~equal:Location.equal then acc else x.location :: acc)
      |> List.rev
    | Yield { cash; fund; _ } -> [ cash; fund ]
    | Hedge { staked; position; _ } -> [ staked; position ]
  ;;
end

module Drawdown = struct
  module Stable = struct
    module V1 = struct
      type t =
        { asset : Currency.Stable.V1.t
        ; max_drawdown_bps : int
        ; lookback : Time_ns.Span.Stable.V2.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    { asset : Currency.t
    ; max_drawdown_bps : int
    ; lookback : Time_ns.Span.t
    }
  [@@deriving sexp, compare, equal]
end

module Guards = struct
  module Stable = struct
    module V1 = struct
      type t =
        { daily_cap : Amount.Stable.V1.t option [@sexp.option]
        ; approval_above : Amount.Stable.V1.t option [@sexp.option]
        ; drawdown : Drawdown.Stable.V1.t option [@sexp.option]
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    { daily_cap : Amount.t option [@sexp.option]
    ; approval_above : Amount.t option [@sexp.option]
    ; drawdown : Drawdown.t option [@sexp.option]
    }
  [@@deriving sexp, compare, equal]

  let none = { daily_cap = None; approval_above = None; drawdown = None }
end

module Status = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Active
        | Paused
        | Archived
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Active
    | Paused
    | Archived
  [@@deriving sexp, compare, equal, enumerate]
end

module Repr = struct
  type t =
    { id : Rule_id.Stable.V1.t
    ; name : string
    ; kind : Kind.Stable.V1.t
    ; guards : Guards.Stable.V1.t
    ; cadence : Cadence.Stable.V1.t
    ; mode : Mode.Stable.V1.t
    ; status : Status.Stable.V1.t
    ; revision : int
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { id : Rule_id.t
  ; name : string
  ; kind : Kind.t
  ; guards : Guards.t
  ; cadence : Cadence.t
  ; mode : Mode.t
  ; status : Status.t
  ; revision : int
  }
[@@deriving sexp_of, compare, equal]

let error what fields = Or_error.error_s (Sexp.List [ Sexp.Atom what; fields ])
let check cond what fields = if cond then Ok () else error what fields

let same_currency what amounts =
  match amounts with
  | [] -> Ok ()
  | first :: _ ->
    let c = Amount.currency first in
    check
      (List.for_all amounts ~f:(fun a -> Currency.equal (Amount.currency a) c))
      ("currencies differ: " ^ what)
      [%sexp (amounts : Amount.t list)]
;;

let non_negative what a =
  check (not (Amount.is_negative a)) ("negative " ^ what) [%sexp (a : Amount.t)]
;;

let positive what a =
  check (Amount.is_positive a) (what ^ " must be positive") [%sexp (a : Amount.t)]
;;

let bps what ~lo ~hi v =
  check
    (v >= lo && v <= hi)
    (sprintf "%s out of range [%d, %d] bps" what lo hi)
    [%sexp (v : int)]
;;

let distinct what a b =
  check
    (not (Location.equal a b))
    (what ^ ": source and destination are the same")
    [%sexp (a : Location.t)]
;;

let validate_kind : Kind.t -> unit Or_error.t =
  let open Or_error.Let_syntax in
  function
  | Sweep { from; to_; threshold; minimum } ->
    let%bind () = distinct "sweep" from to_ in
    let%bind () = same_currency "sweep" [ threshold; minimum ] in
    let%bind () = non_negative "threshold" threshold in
    non_negative "minimum" minimum
  | Top_up { from; to_; floor; target; minimum } ->
    let%bind () = distinct "top up" from to_ in
    let%bind () = same_currency "top up" [ floor; target; minimum ] in
    let%bind () = non_negative "floor" floor in
    let%bind () = non_negative "minimum" minimum in
    check
      (Amount.( >= ) target floor)
      "top up target below floor"
      [%sexp { floor : Amount.t; target : Amount.t }]
  | Convert { from = _; to_ = _; threshold; buy; minimum; max_per_run } ->
    let%bind () =
      same_currency "convert" ([ threshold; minimum ] @ Option.to_list max_per_run)
    in
    let%bind () = non_negative "threshold" threshold in
    let%bind () = non_negative "minimum" minimum in
    let%bind () =
      match max_per_run with
      | None -> Ok ()
      | Some m -> positive "max_per_run" m
    in
    check
      (not (Currency.equal buy (Amount.currency threshold)))
      "convert buys the asset it sells"
      [%sexp (buy : Currency.t)]
  | Rebalance { targets; band_bps; minimum } ->
    let%bind () =
      check (List.length targets >= 2) "rebalance needs two legs or more" [%sexp ()]
    in
    let%bind () = bps "band" ~lo:1 ~hi:10_000 band_bps in
    let%bind () = non_negative "minimum" minimum in
    let%bind () =
      Or_error.combine_errors_unit
        (List.map targets ~f:(fun (x : Target.t) ->
           bps "weight" ~lo:0 ~hi:10_000 x.weight_bps))
    in
    let keys =
      List.map targets ~f:(fun (x : Target.t) ->
        Location.to_string x.location ^ " " ^ Currency.to_string x.asset)
    in
    let%bind () =
      check
        (not (List.contains_dup keys ~compare:String.compare))
        "rebalance legs repeat"
        [%sexp (keys : string list)]
    in
    let total = List.sum (module Int) targets ~f:(fun x -> x.weight_bps) in
    check (total = 10_000) "rebalance weights do not sum to 10000" [%sexp (total : int)]
  | Yield { cash; fund; buffer; max_share_bps; minimum } ->
    let%bind () = distinct "yield" cash fund in
    let%bind () = same_currency "yield" [ buffer; minimum ] in
    let%bind () = non_negative "buffer" buffer in
    let%bind () = non_negative "minimum" minimum in
    bps "max_share" ~lo:0 ~hi:10_000 max_share_bps
  | Hedge { asset; staked; position; ratio_bps; band_bps; max_position; min_notional } ->
    let%bind () = distinct "hedge" staked position in
    let%bind () =
      match max_position with
      | Some m when not (Currency.equal (Amount.currency m) asset) ->
        error "hedge max_position is not in the hedged asset" [%sexp (m : Amount.t)]
      | _ -> Ok ()
    in
    let%bind () = bps "ratio" ~lo:0 ~hi:10_000 ratio_bps in
    let%bind () = bps "band" ~lo:0 ~hi:10_000 band_bps in
    let%bind () = non_negative "min_notional" min_notional in
    (match max_position with
     | None -> Ok ()
     | Some m -> positive "max_position" m)
;;

let validate_guards ({ daily_cap; approval_above; drawdown } : Guards.t) =
  let open Or_error.Let_syntax in
  let%bind () = Option.value_map daily_cap ~default:(Ok ()) ~f:(positive "daily_cap") in
  let%bind () =
    Option.value_map approval_above ~default:(Ok ()) ~f:(non_negative "approval_above")
  in
  match drawdown with
  | None -> Ok ()
  | Some { asset = _; max_drawdown_bps; lookback } ->
    let%bind () = bps "max_drawdown" ~lo:1 ~hi:10_000 max_drawdown_bps in
    check
      (Time_ns.Span.( >= ) lookback Time_ns.Span.minute)
      "drawdown lookback shorter than a minute"
      [%sexp (lookback : Time_ns.Span.t)]
;;

let validate t =
  let open Or_error.Let_syntax in
  let%bind () =
    check
      (String.length t.name >= 1 && String.length t.name <= 80)
      "rule name must have 1 to 80 characters"
      [%sexp (t.name : string)]
  in
  let%bind () = check (t.revision >= 1) "revision below 1" [%sexp (t.revision : int)] in
  let%bind () = validate_kind t.kind in
  validate_guards t.guards
  |> Or_error.tag_s ~tag:[%message "invalid rule" ~id:(t.id : Rule_id.t)]
;;

let checked t = Or_error.map (validate t) ~f:(fun () -> t)

let create
  ~id
  ~name
  ~kind
  ?(guards = Guards.none)
  ?(cadence = Cadence.daily)
  ?(mode = Mode.Dry_run)
  ()
  =
  checked { id; name; kind; guards; cadence; mode; status = Active; revision = 1 }
;;

let not_archived t =
  match t.status with
  | Archived -> error "rule is archived" [%sexp (t.id : Rule_id.t)]
  | Active | Paused -> Ok ()
;;

let update t ?name ?kind ?guards ?cadence ?mode () =
  let open Or_error.Let_syntax in
  let%bind () = not_archived t in
  checked
    { t with
      name = Option.value name ~default:t.name
    ; kind = Option.value kind ~default:t.kind
    ; guards = Option.value guards ~default:t.guards
    ; cadence = Option.value cadence ~default:t.cadence
    ; mode = Option.value mode ~default:t.mode
    ; revision = t.revision + 1
    }
;;

let set_status t ~from status =
  if Status.equal t.status from
  then Ok { t with status; revision = t.revision + 1 }
  else
    error
      "rule status does not allow this change"
      [%sexp
        { id : Rule_id.t = t.id; status : Status.t = t.status; to_ : Status.t = status }]
;;

let pause t = set_status t ~from:Active Paused
let resume t = set_status t ~from:Paused Active

let archive t =
  Or_error.map (not_archived t) ~f:(fun () ->
    { t with status = Archived; revision = t.revision + 1 })
;;

let is_scheduled t =
  match t.status with
  | Active -> Mode.is_scheduled t.mode
  | Paused | Archived -> false
;;

let of_repr r = Or_error.ok_exn (checked r)
let t_of_sexp sexp = of_repr (Repr.t_of_sexp sexp)

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, sexp_of]

    let t_of_sexp = t_of_sexp

    (* Validate on read: a stored rule that no longer passes [validate] is an error at
       load time, not a wrong action at run time. *)
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
