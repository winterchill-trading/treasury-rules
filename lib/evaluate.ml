open! Core
open Money

type ctx =
  { settings : Settings.t
  ; snapshot : Snapshot.t
  ; now : Time_ns.t
  ; rule : Rule.t
  }

let amt = Amount.to_string
let loc = Location.to_string
let cur = Currency.to_string
let block = Hit.block
let ( let* ) r f = Result.bind r ~f
let ( let+ ) r f = Result.map r ~f
let min_a a b = if Amount.( <= ) a b then a else b
let max_a a b = if Amount.( >= ) a b then a else b
let bps_of_int n = Q.of_ints n 10_000

let balance ctx l c =
  match Snapshot.balance ctx.snapshot l c with
  | Some a -> Ok a
  | None ->
    Error
      (block
         No_balance
         (sprintf "no %s balance at %s" (cur c) (loc l))
         [ "location", loc l; "asset", cur c ])
;;

let rate ctx ~base ~quote =
  match
    Snapshot.rate
      ctx.snapshot
      ~base
      ~quote
      ~via:ctx.settings.valuation
      ~now:ctx.now
      ~max_age:ctx.settings.max_price_age
  with
  | Some r -> Ok r
  | None ->
    Error
      (block
         Price_unavailable
         (sprintf "no %s/%s price within the allowed age" (cur base) (cur quote))
         [ "pair", cur base ^ "/" ^ cur quote
         ; "max_age", Time_ns.Span.to_string ctx.settings.max_price_age
         ])
;;

let value ctx a =
  let v = ctx.settings.valuation in
  if Currency.equal (Amount.currency a) v
  then Ok a
  else
    let+ r = rate ctx ~base:(Amount.currency a) ~quote:v in
    Rate.convert ~rounding:Down r a |> Or_error.ok_exn
;;

(* Converts an amount of the valuation currency into [c]. *)
let in_asset ctx a c ~rounding =
  if Currency.equal (Amount.currency a) c
  then Ok a
  else
    let+ r = rate ctx ~base:(Amount.currency a) ~quote:c in
    Rate.convert ~rounding r a |> Or_error.ok_exn
;;

let require_valuation ctx what a =
  if Currency.equal (Amount.currency a) ctx.settings.valuation
  then Ok ()
  else
    Error
      (block
         Rule_error
         (sprintf
            "%s is not in the valuation currency %s"
            what
            (cur ctx.settings.valuation))
         [ what, amt a ])
;;

let action ~kind ~reason ~value ~minimum : Action.t = { kind; reason; value; minimum }

let sweep ctx ~from ~to_ ~threshold ~minimum =
  let* b = balance ctx from (Amount.currency threshold) in
  let excess = Amount.sub_exn b threshold in
  let ev = [ "balance", amt b; "threshold", amt threshold ] in
  if not (Amount.is_positive excess)
  then Error (block Below_threshold "balance at or below the threshold" ev)
  else if Amount.( < ) excess minimum
  then
    Error
      (block
         Below_minimum
         "excess below the minimum"
         (ev @ [ "excess", amt excess; "minimum", amt minimum ]))
  else
    let+ value = value ctx excess in
    ( [ action
          ~kind:(Transfer { from; to_; amount = excess })
          ~reason:Sweep_excess
          ~value
          ~minimum
      ]
    , [ Hit.action
          Sweep_excess
          "excess above the threshold"
          (ev @ [ "excess", amt excess ])
      ] )
;;

let top_up ctx ~from ~to_ ~floor ~target ~minimum =
  let c = Amount.currency floor in
  let* dst = balance ctx to_ c in
  let* src = balance ctx from c in
  let ev = [ "balance", amt dst; "floor", amt floor; "target", amt target ] in
  if Amount.( >= ) dst floor
  then Error (block Floor_met "balance at or above the floor" ev)
  else (
    let need = Amount.sub_exn target dst in
    let available = max_a src (Amount.zero c) in
    let amount = min_a need available in
    let ev = ev @ [ "need", amt need; "source", amt src ] in
    if (not (Amount.is_positive amount))
       || (Amount.( < ) amount minimum && Amount.( < ) available need)
    then Error (block Insufficient_balance "the source cannot fund the top-up" ev)
    else if Amount.( < ) amount minimum
    then
      Error
        (block Below_minimum "top-up below the minimum" (ev @ [ "minimum", amt minimum ]))
    else
      let+ value = value ctx amount in
      ( [ action
            ~kind:(Transfer { from; to_; amount })
            ~reason:Top_up_floor
            ~value
            ~minimum
        ]
      , [ Hit.action Top_up_floor "balance below the floor" (ev @ [ "amount", amt amount ])
        ] ))
;;

let convert ctx ~from ~to_ ~threshold ~buy ~minimum ~max_per_run =
  let c = Amount.currency threshold in
  let* b = balance ctx from c in
  let excess = Amount.sub_exn b threshold in
  let ev = [ "balance", amt b; "threshold", amt threshold ] in
  if not (Amount.is_positive excess)
  then Error (block Below_threshold "balance at or below the threshold" ev)
  else (
    let sell = Option.value_map max_per_run ~default:excess ~f:(min_a excess) in
    let ev = ev @ [ "sell", amt sell ] in
    if Amount.( < ) sell minimum
    then
      Error
        (block Below_minimum "excess below the minimum" (ev @ [ "minimum", amt minimum ]))
    else
      let* r = rate ctx ~base:c ~quote:buy in
      let buy_estimate = Rate.convert ~rounding:Down r sell |> Or_error.ok_exn in
      let ev =
        ev @ [ "rate", Rate.price_to_string (Rate.price r); "buy", amt buy_estimate ]
      in
      if not (Amount.is_positive buy_estimate)
      then Error (block Below_minimum "the conversion would buy nothing" ev)
      else
        let+ value = value ctx sell in
        ( [ action
              ~kind:(Convert { from; to_; sell; buy_estimate; rate = r })
              ~reason:Convert_excess
              ~value
              ~minimum
          ]
        , [ Hit.action Convert_excess "excess above the threshold is converted" ev ] ))
;;

let drift_text q =
  (* Basis points with two decimals, rounded toward zero, for evidence only. *)
  let hundredths = Rounding.round Toward_zero (Q.mul q (Q.of_int 100)) in
  Decimal_text.to_string hundredths ~digits:2
;;

let rebalance ctx ~(targets : Rule.Target.t list) ~band_bps ~minimum =
  let* () = require_valuation ctx "minimum" minimum in
  let v = ctx.settings.valuation in
  let* legs =
    List.map targets ~f:(fun t ->
      let* b = balance ctx t.location t.asset in
      let+ value = value ctx b in
      t, value)
    |> Result.all
  in
  let total = Amount.sum v (List.map legs ~f:snd) |> Or_error.ok_exn in
  if not (Amount.is_positive total)
  then Error (block Below_threshold "nothing to rebalance" [ "total", amt total ])
  else (
    let drift ((t : Rule.Target.t), value) =
      Q.sub
        (Q.mul (Q.div (Amount.to_q value) (Amount.to_q total)) (Q.of_int 10_000))
        (Q.of_int t.weight_bps)
    in
    let leg_name (t : Rule.Target.t) = loc t.location ^ " " ^ cur t.asset in
    let ev =
      ("total", amt total)
      :: ("band_bps", Int.to_string band_bps)
      :: List.map legs ~f:(fun ((t, value) as leg) ->
        ( leg_name t
        , sprintf "%s, %s bps from %d" (amt value) (drift_text (drift leg)) t.weight_bps ))
    in
    let max_drift =
      List.map legs ~f:(fun leg -> Q.abs (drift leg)) |> List.fold ~init:Q.zero ~f:Q.max
    in
    if Q.leq max_drift (Q.of_int band_bps)
    then Error (block Within_band "every leg is within the band" ev)
    else (
      let diffs =
        List.map legs ~f:(fun ((t, value) : Rule.Target.t * Amount.t) ->
          let target = Amount.mul_ratio ~rounding:Down total (bps_of_int t.weight_bps) in
          t, Amount.sub_exn value target)
      in
      let by_size side =
        List.filter_map diffs ~f:(fun (t, d) ->
          let d = if side then d else Amount.neg d in
          if Amount.is_positive d then Some (t, d) else None)
        |> List.stable_sort ~compare:(fun (_, a) (_, b) -> Amount.compare b a)
      in
      let move (o : Rule.Target.t) (u : Rule.Target.t) m =
        let* amount = in_asset ctx m o.asset ~rounding:Down in
        let* min_o = in_asset ctx minimum o.asset ~rounding:Up in
        let+ (kind : Action.Kind.t) =
          if Currency.equal o.asset u.asset
          then Ok (Action.Kind.Transfer { from = o.location; to_ = u.location; amount })
          else
            let+ r = rate ctx ~base:o.asset ~quote:u.asset in
            Action.Kind.Convert
              { from = o.location
              ; to_ = u.location
              ; sell = amount
              ; buy_estimate = Rate.convert ~rounding:Down r amount |> Or_error.ok_exn
              ; rate = r
              }
        in
        action ~kind ~reason:Rebalance_drift ~value:m ~minimum:min_o
      in
      let rec pair overs unders acc =
        match overs, unders with
        | (o, s) :: os, (u, d) :: us ->
          let m = min_a s d in
          let acc =
            if Amount.is_positive m && Amount.( >= ) m minimum
            then move o u m :: acc
            else acc
          in
          let s' = Amount.sub_exn s m
          and d' = Amount.sub_exn d m in
          pair
            (if Amount.is_positive s' then (o, s') :: os else os)
            (if Amount.is_positive d' then (u, d') :: us else us)
            acc
        | _ -> List.rev acc
      in
      let* moves = pair (by_size true) (by_size false) [] |> Result.all in
      match moves with
      | [] -> Error (block Below_minimum "every move is below the minimum" ev)
      | moves ->
        Ok (moves, [ Hit.action Rebalance_drift "a leg drifted outside the band" ev ])))
;;

let yield_ ctx ~cash ~fund ~buffer ~max_share_bps ~minimum =
  let c = Amount.currency buffer in
  let* cash_b = balance ctx cash c in
  let* fund_b = balance ctx fund c in
  let total = Amount.add_exn cash_b fund_b in
  let limit = Amount.mul_ratio ~rounding:Down total (bps_of_int max_share_bps) in
  let ev =
    [ "cash", amt cash_b; "fund", amt fund_b; "buffer", amt buffer; "limit", amt limit ]
  in
  let propose kind reason detail amount =
    if Amount.( < ) amount minimum
    then
      Error
        (block
           Below_minimum
           (detail ^ ", but below the minimum")
           (ev @ [ "amount", amt amount; "minimum", amt minimum ]))
    else
      let+ value = value ctx amount in
      ( [ action ~kind ~reason ~value ~minimum ]
      , [ Hit.action reason detail (ev @ [ "amount", amt amount ]) ] )
  in
  if Amount.( < ) cash_b buffer
  then (
    let r = min_a (Amount.sub_exn buffer cash_b) (max_a fund_b (Amount.zero c)) in
    if not (Amount.is_positive r)
    then Error (block Below_buffer "cash below the buffer and nothing to redeem" ev)
    else
      propose
        (Redeem { fund; to_ = cash; amount = r })
        Yield_redeem_liquidity
        "cash below the buffer"
        r)
  else if Amount.( > ) fund_b limit
  then
    propose
      (Redeem { fund; to_ = cash; amount = Amount.sub_exn fund_b limit })
      Yield_redeem_over_limit
      "yield above its allocation limit"
      (Amount.sub_exn fund_b limit)
  else (
    let idle = Amount.sub_exn cash_b buffer in
    let room = Amount.sub_exn limit fund_b in
    if not (Amount.is_positive room)
    then Error (block Allocation_limit_reached "yield at its allocation limit" ev)
    else if not (Amount.is_positive idle)
    then Error (block Below_buffer "no cash above the buffer" ev)
    else (
      let s = min_a idle room in
      propose
        (Subscribe { from = cash; fund; amount = s })
        Yield_subscribe
        "cash above the buffer"
        s))
;;

let hedge ctx ~asset ~staked ~position ~ratio_bps ~band_bps ~max_position ~min_notional =
  let* () = require_valuation ctx "min_notional" min_notional in
  let* s = balance ctx staked asset in
  let* p = balance ctx position asset in
  let long = max_a s (Amount.zero asset) in
  let unclamped =
    Amount.neg (Amount.mul_ratio ~rounding:Down long (bps_of_int ratio_bps))
  in
  let target, clamped =
    match max_position with
    | Some m when Amount.( < ) unclamped (Amount.neg m) -> Amount.neg m, true
    | _ -> unclamped, false
  in
  let drift = Amount.sub_exn target p in
  let band = Amount.mul_ratio ~rounding:Down (Amount.abs target) (bps_of_int band_bps) in
  let ev =
    [ "staked", amt s
    ; "position", amt p
    ; "target", amt target
    ; "drift", amt drift
    ; "band", amt band
    ]
  in
  if Amount.( <= ) (Amount.abs drift) band
  then
    if clamped
    then Error (block Position_limit_reached "the hedge is at its maximum position" ev)
    else Error (block Within_band "the hedge is within its band" ev)
  else
    let* r = rate ctx ~base:asset ~quote:ctx.settings.valuation in
    let size = Amount.abs drift in
    let notional = Rate.convert ~rounding:Down r size |> Or_error.ok_exn in
    let ev =
      ev @ [ "price", Rate.price_to_string (Rate.price r); "notional", amt notional ]
    in
    if Amount.( < ) notional min_notional
    then
      Error
        (block
           Below_minimum
           "order below the minimum notional"
           (ev @ [ "min_notional", amt min_notional ]))
    else (
      let minimum =
        Amount.of_q ~rounding:Up asset (Q.div (Amount.to_q min_notional) (Rate.price r))
      in
      let side : Action.Side.t = if Amount.is_negative drift then Sell else Buy in
      Ok
        ( [ action
              ~kind:(Hedge { position; side; size; price = r })
              ~reason:Hedge_adjust
              ~value:notional
              ~minimum
          ]
        , [ Hit.action Hedge_adjust "the hedge drifted outside its band" ev ] ))
;;

let propose ctx =
  match ctx.rule.kind with
  | Sweep { from; to_; threshold; minimum } -> sweep ctx ~from ~to_ ~threshold ~minimum
  | Top_up { from; to_; floor; target; minimum } ->
    top_up ctx ~from ~to_ ~floor ~target ~minimum
  | Convert { from; to_; threshold; buy; minimum; max_per_run } ->
    convert ctx ~from ~to_ ~threshold ~buy ~minimum ~max_per_run
  | Rebalance { targets; band_bps; minimum } -> rebalance ctx ~targets ~band_bps ~minimum
  | Yield { cash; fund; buffer; max_share_bps; minimum } ->
    yield_ ctx ~cash ~fund ~buffer ~max_share_bps ~minimum
  | Hedge { asset; staked; position; ratio_bps; band_bps; max_position; min_notional } ->
    hedge ctx ~asset ~staked ~position ~ratio_bps ~band_bps ~max_position ~min_notional
;;

(* ---- guards ---- *)

let check_active ctx =
  match ctx.rule.status with
  | Active -> Ok ()
  | (Paused | Archived) as status ->
    Error
      (block
         Rule_inactive
         "the rule is not active"
         [ "status", Sexp.to_string [%sexp (status : Rule.Status.t)] ])
;;

let check_fresh ctx =
  let oldest = Time_ns.sub ctx.now ctx.settings.max_snapshot_age in
  if Time_ns.( >= ) ctx.snapshot.at oldest
  then Ok ()
  else
    Error
      (block
         Snapshot_stale
         "balances are older than allowed"
         [ "at", Time_ns.to_string_utc ctx.snapshot.at
         ; "max_age", Time_ns.Span.to_string ctx.settings.max_snapshot_age
         ])
;;

let check_pending ctx =
  let current = Run_key.create ctx.rule.id ctx.rule.cadence ~now:ctx.now in
  match
    List.find ctx.snapshot.runs ~f:(fun r ->
      Rule_id.equal r.key.rule ctx.rule.id
      && (not (Run_key.equal r.key current))
      && Run_record.is_pending r)
  with
  | None -> Ok ()
  | Some r ->
    Error
      (block
         Previous_pending
         "an earlier run still has unfinished actions"
         [ "run", Run_key.to_string r.key
         ; "status", Sexp.to_string [%sexp (r.status : Run_record.Status.t)]
         ])
;;

let check_drawdown ctx =
  match ctx.rule.guards.drawdown with
  | None -> Ok ()
  | Some { asset; max_drawdown_bps; lookback } ->
    let v = ctx.settings.valuation in
    let* current = rate ctx ~base:asset ~quote:v in
    let since = Time_ns.sub ctx.now lookback in
    let observed =
      List.filter_map (ctx.snapshot.price_history @ ctx.snapshot.prices) ~f:(fun r ->
        if Time_ns.( < ) (Rate.at r) since
        then None
        else if Currency.equal (Rate.base r) asset && Currency.equal (Rate.quote r) v
        then Some (Rate.price r)
        else if Currency.equal (Rate.base r) v && Currency.equal (Rate.quote r) asset
        then Some (Q.inv (Rate.price r))
        else None)
    in
    let now_price = Rate.price current in
    let peak = List.fold observed ~init:now_price ~f:Q.max in
    let drawdown = Q.mul (Q.div (Q.sub peak now_price) peak) (Q.of_int 10_000) in
    let ev =
      [ "asset", cur asset
      ; "peak", Rate.price_to_string peak
      ; "current", Rate.price_to_string now_price
      ; "drawdown_bps", drift_text drawdown
      ; "max_drawdown_bps", Int.to_string max_drawdown_bps
      ]
    in
    if Q.gt drawdown (Q.of_int max_drawdown_bps)
    then Error (block Drawdown_pause "the reference price fell more than allowed" ev)
    else Ok ()
;;

(* ---- caps ---- *)

let day t = Time_ns.to_date t ~zone:Time_float.Zone.utc

let sum_today (settings : Settings.t) (snapshot : Snapshot.t) ~now ~f =
  let today = day now in
  List.filter snapshot.runs ~f:(fun r ->
    f r && Date.equal (day r.Run_record.key.period_start) today)
  |> List.concat_map ~f:Run_record.actions
  |> List.filter_map ~f:(fun (a : Action.t) ->
    if Currency.equal (Amount.currency a.value) settings.valuation
    then Some a.value
    else None)
  |> Amount.sum settings.valuation
  |> Or_error.ok_exn
;;

let used_today settings snapshot ~now ~rule =
  sum_today settings snapshot ~now ~f:(fun r ->
    Run_record.counts_against_caps r
    && Option.value_map rule ~default:true ~f:(Rule_id.equal r.key.rule))
;;

let apply_cap ~code ~cap ~used (actions : Action.t list) =
  let remaining = Amount.sub_exn cap used in
  let ev = [ "cap", amt cap; "used", amt used ] in
  let reached () = Error (block code "the daily cap is used up" ev) in
  if not (Amount.is_positive remaining)
  then reached ()
  else (
    let kept, cut, _ =
      List.fold actions ~init:([], false, remaining) ~f:(fun (kept, cut, rem) a ->
        if Amount.( <= ) a.value rem
        then a :: kept, cut, Amount.sub_exn rem a.value
        else if Amount.is_positive rem
        then (
          match Action.scale a (Q.div (Amount.to_q rem) (Amount.to_q a.value)) with
          | Some a' -> a' :: kept, true, Amount.sub_exn rem a'.value
          | None -> kept, true, rem)
        else kept, true, rem)
    in
    match List.rev kept with
    | [] -> reached ()
    | kept ->
      let hits =
        if cut
        then
          [ Hit.action
              Daily_cap_applied
              "actions reduced to fit the daily cap"
              (ev @ [ "cap_code", Reason_code.to_string code ])
          ]
        else []
      in
      Ok (kept, hits))
;;

let caps ctx actions =
  let { settings; snapshot; now; rule } = ctx in
  let* actions, rule_hits =
    match rule.guards.daily_cap with
    | None -> Ok (actions, [])
    | Some cap ->
      let* () = require_valuation ctx "daily_cap" cap in
      let used =
        match rule.mode with
        | Live -> used_today settings snapshot ~now ~rule:(Some rule.id)
        | Shadow | Dry_run ->
          sum_today settings snapshot ~now ~f:(fun r ->
            Rule_id.equal r.key.rule rule.id && Run_record.Status.equal r.status Shadowed)
      in
      apply_cap ~code:Rule_daily_cap_reached ~cap ~used actions
  in
  let+ actions, org_hits =
    match settings.org_daily_cap with
    | None -> Ok (actions, [])
    | Some cap ->
      let used = used_today settings snapshot ~now ~rule:None in
      apply_cap ~code:Org_daily_cap_reached ~cap ~used actions
  in
  actions, rule_hits @ org_hits
;;

let routing ctx (actions : Action.t list) =
  let manual =
    if ctx.settings.auto_execute
    then []
    else [ Hit.review Manual_execution "automatic execution is off" [] ]
  in
  let+ above =
    match ctx.rule.guards.approval_above with
    | None -> Ok []
    | Some threshold ->
      let* () = require_valuation ctx "approval_above" threshold in
      (match List.filter actions ~f:(fun a -> Amount.( > ) a.value threshold) with
       | [] -> Ok []
       | big ->
         Ok
           [ Hit.review
               Approval_required
               "an action is above the approval threshold"
               [ "threshold", amt threshold
               ; ( "largest"
                 , amt
                     (List.map big ~f:(fun a -> a.value)
                      |> List.max_elt ~compare:Amount.compare
                      |> Option.value_exn) )
               ; "count", Int.to_string (List.length big)
               ]
           ])
  in
  manual @ above
;;

let evaluate ?mode settings (rule : Rule.t) snapshot ~now =
  let ctx = { settings; snapshot; now; rule } in
  let result =
    try
      let* () =
        Settings.validate settings
        |> Result.map_error ~f:(fun e ->
          block Rule_error "invalid settings" [ "error", Error.to_string_hum e ])
      in
      let* () = check_active ctx in
      let* () = check_fresh ctx in
      let* () = check_pending ctx in
      let* () = check_drawdown ctx in
      let* actions, hits = propose ctx in
      let* actions, cap_hits = caps ctx actions in
      let+ route_hits = routing ctx actions in
      actions, hits @ cap_hits @ route_hits
    with
    | exn -> Error (block Rule_error "the rule raised" [ "exn", Exn.to_string exn ])
  in
  let actions, hits =
    match result with
    | Ok x -> x
    | Error hit -> [], [ hit ]
  in
  Outcome.create
    ~rule:rule.id
    ~revision:rule.revision
    ~mode:(Option.value mode ~default:rule.mode)
    ~evaluated_at:now
    ~actions
    ~hits
;;
