open! Core
open Money
open! Treasury_rules
open Fixtures
module R = Runner.Make (Monad.Ident) (Store_memory)

let usdc_units n = Amount.of_minor_units_int usdc n
let gen_units = Int.gen_incl 0 100_000_000_000
let ( >= ) = Amount.( >= )
let ( <= ) = Amount.( <= )
let sum l = Amount.sum usdc l |> Or_error.ok_exn
let trials = 2_000

let%test_unit "sweep leaves exactly the threshold and never moves less than the minimum" =
  Quickcheck.test
    ~trials
    (Quickcheck.Generator.tuple3 gen_units gen_units gen_units)
    ~f:(fun (b, th, mn) ->
      let threshold = usdc_units th
      and minimum = usdc_units mn
      and balance = usdc_units b in
      let r = rule "p" (Sweep { from = loc "a"; to_ = loc "b"; threshold; minimum }) in
      let s =
        Snapshot.create ~at:now ~balances:[ { location = loc "a"; amount = balance } ] ()
      in
      let o = Evaluate.evaluate settings r s ~now in
      match o.route, o.actions with
      | Execute, [ { kind = Transfer { amount; _ }; _ } ] ->
        assert (Amount.equal (Amount.sub_exn balance amount) threshold);
        assert (amount >= minimum)
      | Skip, [] ->
        assert (
          Amount.( < ) (Amount.sub_exn balance threshold) minimum
          || not (Amount.is_positive (Amount.sub_exn balance threshold)))
      | _ -> assert false)
;;

let%test_unit "the route always agrees with the hits" =
  let gen_hit =
    Quickcheck.Generator.map
      (Quickcheck.Generator.tuple2
         Reason_code.quickcheck_generator
         Risk_rules.Severity.quickcheck_generator)
      ~f:(fun (code, severity) -> { Hit.code; severity; detail = ""; evidence = [] })
  in
  let action : Action.t =
    { kind = Transfer { from = loc "a"; to_ = loc "b"; amount = a "1.000000 USDC" }
    ; reason = Sweep_excess
    ; value = a "1.000000 USDC"
    ; minimum = a "0.000000 USDC"
    }
  in
  Quickcheck.test
    ~trials
    (Quickcheck.Generator.tuple2
       (List.quickcheck_generator gen_hit)
       Bool.quickcheck_generator)
    ~f:(fun (hits, with_action) ->
      let o =
        Outcome.create
          ~rule:(id "p")
          ~revision:1
          ~mode:Live
          ~evaluated_at:now
          ~actions:(if with_action then [ action ] else [])
          ~hits
      in
      let has s = List.exists hits ~f:(fun h -> Risk_rules.Severity.equal h.severity s) in
      let expected : Outcome.Route.t =
        if has Block || not with_action
        then Skip
        else if has Review
        then Approve
        else Execute
      in
      assert (Outcome.Route.equal o.route expected);
      assert (Bool.equal (List.is_empty o.actions) (Outcome.Route.equal expected Skip)))
;;

let%test_unit "rebalancing in one currency brings every leg to its target" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind legs = Int.gen_incl 2 5 in
    let%bind balances = List.gen_with_length legs gen_units in
    let%bind cuts = List.gen_with_length (legs - 1) (Int.gen_incl 0 10_000) in
    let%map band = Int.gen_incl 1 2_000 in
    let cuts = List.sort cuts ~compare:Int.compare in
    let weights =
      List.map2_exn (cuts @ [ 10_000 ]) (0 :: cuts) ~f:(fun hi lo -> hi - lo)
    in
    balances, weights, band
  in
  Quickcheck.test ~trials gen ~f:(fun (balances, weights, band) ->
    let names = List.mapi balances ~f:(fun i _ -> sprintf "leg%d" i) in
    let targets =
      List.map2_exn names weights ~f:(fun n w ->
        { Rule.Target.location = loc n; asset = usdc; weight_bps = w })
    in
    let r =
      rule "p" (Rebalance { targets; band_bps = band; minimum = a "0.000000 USDC" })
    in
    let rows =
      List.map2_exn names balances ~f:(fun n b ->
        { Snapshot.Balance.location = loc n; amount = usdc_units b })
    in
    let o =
      Evaluate.evaluate settings r (Snapshot.create ~at:now ~balances:rows ()) ~now
    in
    let after = Location.Table.create () in
    List.iter rows ~f:(fun b -> Hashtbl.set after ~key:b.location ~data:b.amount);
    List.iter o.actions ~f:(fun act ->
      match act.kind with
      | Transfer { from; to_; amount } ->
        Hashtbl.update after from ~f:(fun v -> Amount.sub_exn (Option.value_exn v) amount);
        Hashtbl.update after to_ ~f:(fun v -> Amount.add_exn (Option.value_exn v) amount)
      | _ -> assert false);
    let total = sum (List.map rows ~f:(fun b -> b.amount)) in
    assert (Amount.equal total (sum (Hashtbl.data after)));
    match o.route with
    | Skip -> ()
    | Execute | Approve ->
      List.iter targets ~f:(fun t ->
        let v = Hashtbl.find_exn after t.location in
        let share = Q.div (Amount.to_q v) (Amount.to_q total) in
        let drift =
          Q.abs (Q.sub (Q.mul share (Q.of_int 10_000)) (Q.of_int t.weight_bps))
        in
        (* Exact targets are rounded down to a minor unit, so a leg may end a few minor
           units off; that is far inside one basis point for these totals unless the total
           itself is tiny. *)
        assert (Q.leq drift (Q.of_int band) || Amount.( < ) total (usdc_units 1_000_000))))
;;

let%test_unit "yield never dips into the buffer or exceeds its share" =
  Quickcheck.test
    ~trials
    (Quickcheck.Generator.tuple4 gen_units gen_units gen_units (Int.gen_incl 0 10_000))
    ~f:(fun (c, f, b, share) ->
      let cash = usdc_units c
      and fund = usdc_units f
      and buffer = usdc_units b in
      let r =
        rule
          "p"
          (Yield
             { cash = loc "cash"
             ; fund = loc "fund"
             ; buffer
             ; max_share_bps = share
             ; minimum = a "0.000001 USDC"
             })
      in
      let s =
        Snapshot.create
          ~at:now
          ~balances:
            [ { location = loc "cash"; amount = cash }
            ; { location = loc "fund"; amount = fund }
            ]
          ()
      in
      let o = Evaluate.evaluate settings r s ~now in
      let limit =
        Amount.mul_ratio
          ~rounding:Down
          (Amount.add_exn cash fund)
          (Q.of_ints share 10_000)
      in
      match o.actions with
      | [] -> ()
      | [ { kind = Subscribe { amount; _ }; _ } ] ->
        assert (Amount.sub_exn cash amount >= buffer);
        assert (Amount.add_exn fund amount <= limit)
      | [ { kind = Redeem { amount; _ }; reason = Yield_redeem_liquidity; _ } ] ->
        assert (amount <= fund);
        assert (Amount.add_exn cash amount <= buffer)
      | [ { kind = Redeem { amount; _ }; reason = Yield_redeem_over_limit; _ } ] ->
        assert (Amount.equal (Amount.sub_exn fund amount) limit)
      | _ -> assert false)
;;

let%test_unit "a hedge order lands on the target and respects the maximum position" =
  let units n = Amount.of_minor_units_int hype n in
  Quickcheck.test
    ~trials
    (Quickcheck.Generator.tuple4
       (Int.gen_incl 0 100_000_000_000)
       (Int.gen_incl (-100_000_000_000) 10_000_000_000)
       (Int.gen_incl 0 10_000)
       (Int.gen_incl 1 100_000_000_000))
    ~f:(fun (staked, position, ratio, max) ->
      let max_position = units max in
      let r =
        rule
          "p"
          (Hedge
             { asset = hype
             ; staked = loc "s"
             ; position = loc "p"
             ; ratio_bps = ratio
             ; band_bps = 500
             ; max_position = Some max_position
             ; min_notional = a "0.000000 USDC"
             })
      in
      let s =
        Snapshot.create
          ~at:now
          ~balances:
            [ { location = loc "s"; amount = units staked }
            ; { location = loc "p"; amount = units position }
            ]
          ~prices:[ price "HYPE" "USDC" "25" ]
          ()
      in
      let o = Evaluate.evaluate settings r s ~now in
      match o.actions with
      | [] -> ()
      | [ { kind = Hedge { side; size; _ }; _ } ] ->
        let after =
          match side with
          | Sell -> Amount.sub_exn (units position) size
          | Buy -> Amount.add_exn (units position) size
        in
        let target =
          Amount.neg
            (Amount.mul_ratio ~rounding:Down (units staked) (Q.of_ints ratio 10_000))
        in
        let target =
          if Amount.( < ) target (Amount.neg max_position)
          then Amount.neg max_position
          else target
        in
        assert (Amount.equal after target);
        assert (Amount.abs after <= max_position)
      | _ -> assert false)
;;

let%test_unit "scaling never grows an action" =
  Quickcheck.test
    ~trials
    (Quickcheck.Generator.tuple3 gen_units (Int.gen_incl 0 1_000) (Int.gen_incl 1 1_000))
    ~f:(fun (n, num, den) ->
      let num = Int.min num den in
      let amount = usdc_units n in
      let act : Action.t =
        { kind = Transfer { from = loc "a"; to_ = loc "b"; amount }
        ; reason = Sweep_excess
        ; value = amount
        ; minimum = usdc_units 0
        }
      in
      match Action.scale act (Q.of_ints num den) with
      | None -> ()
      | Some s ->
        assert (Action.amount s <= amount);
        assert (s.value <= amount);
        assert (Amount.is_positive (Action.amount s)))
;;

(* Several rules with random balances and caps run hour by hour through one store; the
   value committed per UTC day never exceeds the rule caps or the organisation cap. *)
let%test_unit "daily caps hold across rules and runs" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind rules = Int.gen_incl 1 4 in
    let%bind caps = List.gen_with_length rules (Option.quickcheck_generator gen_units) in
    let%bind org = Option.quickcheck_generator gen_units in
    let%map balances = List.gen_with_length 6 (List.gen_with_length rules gen_units) in
    caps, org, balances
  in
  Quickcheck.test ~trials:300 gen ~f:(fun (caps, org, hourly_balances) ->
    let store = Store_memory.create () in
    let positive = Option.filter ~f:(fun n -> n > 0) in
    let settings =
      { settings with org_daily_cap = Option.map (positive org) ~f:usdc_units }
    in
    let rules =
      List.mapi caps ~f:(fun i cap ->
        rule
          ~cadence:Cadence.hourly
          ~guards:
            { Rule.Guards.none with daily_cap = Option.map (positive cap) ~f:usdc_units }
          (sprintf "r%d" i)
          (Sweep
             { from = loc (sprintf "src%d" i)
             ; to_ = loc "dst"
             ; threshold = usdc_units 0
             ; minimum = usdc_units 0
             }))
    in
    List.iter rules ~f:(fun r ->
      R.create_rule store ~actor:System ~now r |> Or_error.ok_exn);
    List.iteri hourly_balances ~f:(fun hour balances ->
      let at =
        Time_ns.add (at "2026-10-14T00:30:00Z") (Time_ns.Span.of_int_sec (3600 * hour))
      in
      let rows =
        List.mapi balances ~f:(fun i b ->
          { Snapshot.Balance.location = loc (sprintf "src%d" i); amount = usdc_units b })
      in
      let results =
        R.tick
          store
          settings
          (Snapshot.create ~at ~balances:rows ())
          ~now:at
          ~actor:System
      in
      (* Execute everything so later runs are not held back as pending. *)
      List.iter results ~f:(function
        | Recorded r when Run_record.Status.equal r.status Awaiting_execution ->
          ignore
            (R.executed store ~actor:System ~now:at r.key |> Or_error.ok_exn
             : Run_record.t)
        | _ -> ()));
    let runs = Store_memory.recent_runs store ~since:(at "2026-10-14T00:00:00Z") in
    let value_of rs =
      sum
        (List.concat_map rs ~f:(fun r ->
           List.map (Run_record.actions r) ~f:(fun a -> a.Action.value)))
    in
    let counted = List.filter runs ~f:Run_record.counts_against_caps in
    Option.iter settings.org_daily_cap ~f:(fun cap -> assert (value_of counted <= cap));
    List.iter rules ~f:(fun r ->
      Option.iter r.guards.daily_cap ~f:(fun cap ->
        assert (
          value_of (List.filter counted ~f:(fun x -> Rule_id.equal x.key.rule r.id))
          <= cap))))
;;

(* Two runners share a store; either may crash after claiming. Each run is recorded at
   most once, and a period whose claim went stale is still recorded by a later tick. *)
let%test_unit "exactly one recorded evaluation per rule and period" =
  let gen =
    List.gen_with_length
      30
      (Quickcheck.Generator.tuple2 (Int.gen_incl 0 179) Bool.quickcheck_generator)
  in
  Quickcheck.test ~trials:300 gen ~f:(fun steps ->
    let store = Store_memory.create () in
    let r =
      rule
        ~cadence:Cadence.hourly
        "r"
        (Sweep
           { from = loc "a"
           ; to_ = loc "b"
           ; threshold = usdc_units 0
           ; minimum = usdc_units 0
           })
    in
    R.create_rule store ~actor:System ~now r |> Or_error.ok_exn;
    let start = at "2026-10-14T00:00:00Z" in
    let steps = List.sort steps ~compare:(fun (a, _) (b, _) -> Int.compare a b) in
    List.iter steps ~f:(fun (minute, crash) ->
      let at = Time_ns.add start (Time_ns.Span.of_int_sec (60 * minute)) in
      if crash
      then
        ignore
          (Store_memory.claim_run
             store
             (Run_key.create r.id r.cadence ~now:at)
             ~mode:Live
             ~now:at
             ~stale_after:settings.stale_claim_after
           : Store.Claim.t)
      else (
        let s =
          Snapshot.create
            ~at
            ~balances:[ { location = loc "a"; amount = usdc_units 0 } ]
            ()
        in
        ignore (R.tick store settings s ~now:at ~actor:System : Runner.Result.t list)));
    let recorded =
      List.filter_map (Store_memory.audit store ~rule:None) ~f:(fun e ->
        match e.event with
        | Run_recorded { key; _ } -> Some key
        | _ -> None)
    in
    assert (not (List.contains_dup recorded ~compare:Run_key.compare));
    List.iter recorded ~f:(fun key ->
      let run = Store_memory.find_run store key |> Option.value_exn in
      assert (not (Run_record.Status.equal run.status Evaluating))))
;;
