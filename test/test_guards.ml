open! Core
open! Treasury_rules
open Fixtures

let sweep ?guards ?mode ?cadence () =
  rule
    ?guards
    ?mode
    ?cadence
    "sweep"
    (Sweep
       { from = loc "exchange:spot"
       ; to_ = loc "bank:reserve"
       ; threshold = a "1000.000000 USDC"
       ; minimum = a "10.000000 USDC"
       })
;;

let rich = [ bal "exchange:spot" "6000.000000 USDC" ]

(* A run of [rule] in the period containing [at], evaluated against [balances] and driven
   to [status]. *)
let past_run ?(settings = settings) rule ~at balances (status : Run_record.Status.t) =
  let key = Run_key.create rule.Rule.id rule.cadence ~now:at in
  let outcome = Evaluate.evaluate settings rule (snapshot ~at balances) ~now:at in
  let r =
    Run_record.record (Run_record.claim key ~mode:rule.mode ~now:at) outcome ~now:at
    |> Or_error.ok_exn
  in
  let ok = Or_error.ok_exn in
  match status with
  | Awaiting_execution | Shadowed | Skipped -> r
  | Executed -> Run_record.executed r ~now:at |> ok
  | Failed -> Run_record.fail r ~reason:"rails refused" ~now:at |> ok
  | Awaiting_approval | Rejected | Evaluating -> failwith "not used"
;;

let%expect_test "a paused rule and a stale snapshot skip" =
  show (Rule.pause (sweep ()) |> Or_error.ok_exn) (snapshot rich);
  show (sweep ()) (snapshot ~at:(at "2026-10-14T09:00:00Z") rich);
  [%expect
    {|
    sweep r2 (live): skip
      [block] RULE_INACTIVE: the rule is not active
        status = Paused
    sweep r1 (live): skip
      [block] SNAPSHOT_STALE: balances are older than allowed
        at = 2026-10-14 09:00:00.000000000Z
        max_age = 15m
    |}]
;;

let%expect_test "an earlier pending run blocks the next period" =
  let rule =
    sweep
      ~guards:{ Rule.Guards.none with approval_above = Some (a "1000.000000 USDC") }
      ()
  in
  let yesterday = at "2026-10-13T10:00:00Z" in
  let key = Run_key.create rule.id rule.cadence ~now:yesterday in
  let outcome =
    Evaluate.evaluate settings rule (snapshot ~at:yesterday rich) ~now:yesterday
  in
  let pending =
    Run_record.record
      (Run_record.claim key ~mode:Live ~now:yesterday)
      outcome
      ~now:yesterday
    |> Or_error.ok_exn
  in
  print_s [%sexp (pending.status : Run_record.Status.t)];
  show rule (snapshot ~runs:[ pending ] rich);
  let rejected = Run_record.reject pending ~note:"not now" ~now |> Or_error.ok_exn in
  show rule (snapshot ~runs:[ rejected ] rich);
  [%expect
    {|
    Awaiting_approval
    sweep r1 (live): skip
      [block] PREVIOUS_PENDING: an earlier run still has unfinished actions
        run = sweep:2026-10-13T00:00:00Z
        status = Awaiting_approval
    sweep r1 (live): approve
      transfer 5000.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
      [review] APPROVAL_REQUIRED: an action is above the approval threshold
        balance = 6000.000000 USDC
        threshold = 1000.000000 USDC
        excess = 5000.000000 USDC
        threshold = 1000.000000 USDC
        largest = 5000.000000 USDC
        count = 1
    |}]
;;

let drawdown =
  { Rule.Guards.none with
    drawdown =
      Some { asset = hype; max_drawdown_bps = 1_000; lookback = Time_ns.Span.of_day 7. }
  }
;;

let%expect_test "drawdown of the reference price pauses the rule" =
  let rule = sweep ~guards:drawdown () in
  let history =
    [ price ~at:(at "2026-10-10T00:00:00Z") "HYPE" "USDC" "30"
    ; price ~at:(at "2026-10-12T00:00:00Z") "HYPE" "USDC" "28"
    ; price ~at:(at "2026-10-01T00:00:00Z") "HYPE" "USDC" "50"
    ]
  in
  (* 30 is the peak inside the 7-day window; 50 is older and ignored. *)
  show rule (snapshot ~prices:[ price "HYPE" "USDC" "26" ] ~history rich);
  show rule (snapshot ~prices:[ price "HYPE" "USDC" "27.5" ] ~history rich);
  show rule (snapshot ~history rich);
  [%expect
    {|
    sweep r1 (live): skip
      [block] DRAWDOWN_PAUSE: the reference price fell more than allowed
        asset = HYPE
        peak = 30
        current = 26
        drawdown_bps = 1333.33
        max_drawdown_bps = 1000
    sweep r1 (live): execute
      transfer 5000.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
        balance = 6000.000000 USDC
        threshold = 1000.000000 USDC
        excess = 5000.000000 USDC
    sweep r1 (live): skip
      [block] PRICE_UNAVAILABLE: no HYPE/USDC price within the allowed age
        pair = HYPE/USDC
        max_age = 15m
    |}]
;;

let capped = { Rule.Guards.none with daily_cap = Some (a "8000.000000 USDC") }

let%expect_test "the rule's daily cap counts earlier runs the same UTC day" =
  let rule = sweep ~guards:capped ~cadence:Cadence.hourly () in
  let morning = at "2026-10-14T08:30:00Z" in
  let earlier = past_run rule ~at:morning rich Executed in
  (* 5000 used; 3000 left, so this run's 5000 is cut to 3000. *)
  show rule (snapshot ~runs:[ earlier ] rich);
  let second =
    past_run
      rule
      ~at:(at "2026-10-14T09:30:00Z")
      [ bal "exchange:spot" "4000.000000 USDC" ]
      Executed
  in
  show rule (snapshot ~runs:[ earlier; second ] rich);
  (* Failed runs and runs from yesterday do not count. *)
  let failed = past_run rule ~at:(at "2026-10-14T07:30:00Z") rich Failed in
  let yesterday = past_run rule ~at:(at "2026-10-13T23:30:00Z") rich Executed in
  show rule (snapshot ~runs:[ failed; yesterday ] rich);
  [%expect
    {|
    sweep r1 (live): execute
      transfer 3000.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
      [allow] DAILY_CAP_APPLIED: actions reduced to fit the daily cap
        balance = 6000.000000 USDC
        threshold = 1000.000000 USDC
        excess = 5000.000000 USDC
        cap = 8000.000000 USDC
        used = 5000.000000 USDC
        cap_code = RULE_DAILY_CAP_REACHED
    sweep r1 (live): skip
      [block] RULE_DAILY_CAP_REACHED: the daily cap is used up
        cap = 8000.000000 USDC
        used = 8000.000000 USDC
    sweep r1 (live): execute
      transfer 5000.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
        balance = 6000.000000 USDC
        threshold = 1000.000000 USDC
        excess = 5000.000000 USDC
    |}]
;;

let%expect_test "the organisation cap counts every live rule" =
  let settings = { settings with org_daily_cap = Some (a "7000.000000 USDC") } in
  let perp_sweep ?mode name =
    rule
      ?mode
      name
      (Sweep
         { from = loc "exchange:perp"
         ; to_ = loc "bank:reserve"
         ; threshold = a "0.000000 USDC"
         ; minimum = a "1.000000 USDC"
         })
  in
  let other = perp_sweep "other" in
  let shadow = perp_sweep ~mode:Shadow "shadow" in
  let morning = at "2026-10-14T08:30:00Z" in
  let perp = [ bal "exchange:perp" "4000.000000 USDC" ] in
  let live_run = past_run ~settings other ~at:morning perp Awaiting_execution in
  let shadow_run = past_run ~settings shadow ~at:morning perp Shadowed in
  show ~settings (sweep ()) (snapshot ~runs:[ live_run; shadow_run ] rich);
  let big =
    past_run
      ~settings
      other
      ~at:morning
      [ bal "exchange:perp" "7000.000000 USDC" ]
      Executed
  in
  show ~settings (sweep ()) (snapshot ~runs:[ big ] rich);
  [%expect
    {|
    sweep r1 (live): execute
      transfer 3000.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
      [allow] DAILY_CAP_APPLIED: actions reduced to fit the daily cap
        balance = 6000.000000 USDC
        threshold = 1000.000000 USDC
        excess = 5000.000000 USDC
        cap = 7000.000000 USDC
        used = 4000.000000 USDC
        cap_code = ORG_DAILY_CAP_REACHED
    sweep r1 (live): skip
      [block] ORG_DAILY_CAP_REACHED: the daily cap is used up
        cap = 7000.000000 USDC
        used = 7000.000000 USDC
    |}]
;;

let%expect_test "a shadow rule's cap counts its own shadow runs" =
  let rule = sweep ~guards:capped ~mode:Shadow ~cadence:Cadence.hourly () in
  let earlier = past_run rule ~at:(at "2026-10-14T08:30:00Z") rich Shadowed in
  print_s [%sexp (earlier.status : Run_record.Status.t)];
  show rule (snapshot ~runs:[ earlier ] rich);
  [%expect
    {|
    Shadowed
    sweep r1 (shadow): execute
      transfer 3000.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
      [allow] DAILY_CAP_APPLIED: actions reduced to fit the daily cap
        balance = 6000.000000 USDC
        threshold = 1000.000000 USDC
        excess = 5000.000000 USDC
        cap = 8000.000000 USDC
        used = 5000.000000 USDC
        cap_code = RULE_DAILY_CAP_REACHED
    |}]
;;

let%expect_test "manual execution routes every proposal to approval" =
  let settings = { settings with auto_execute = false } in
  show ~settings (sweep ()) (snapshot rich);
  [%expect
    {|
    sweep r1 (live): approve
      transfer 5000.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
      [review] MANUAL_EXECUTION: automatic execution is off
        balance = 6000.000000 USDC
        threshold = 1000.000000 USDC
        excess = 5000.000000 USDC
    |}]
;;

let%expect_test "rule errors: valuation mismatch and bad settings" =
  let rule =
    sweep ~guards:{ Rule.Guards.none with daily_cap = Some (a "100.00 EUR") } ()
  in
  show rule (snapshot rich);
  show
    ~settings:{ settings with stale_claim_after = Time_ns.Span.zero }
    (sweep ())
    (snapshot rich);
  [%expect
    {|
    sweep r1 (live): skip
      [block] RULE_ERROR: daily_cap is not in the valuation currency USDC
        daily_cap = 100.00 EUR
    sweep r1 (live): skip
      [block] RULE_ERROR: invalid settings
        error = ("settings: span must be positive" stale_claim_after)
    |}]
;;

let%expect_test "dry-run preview of a live rule and conversion to a risk-rules decision" =
  let o = Evaluate.evaluate ~mode:Dry_run settings (sweep ()) (snapshot rich) ~now in
  print_s [%sexp (o.mode : Mode.t), (o.route : Outcome.Route.t)];
  let d = Outcome.to_decision o ~rule:"sweep" in
  print_endline (Risk_rules.Decision.to_string_hum d);
  let skipped =
    Evaluate.evaluate
      settings
      (sweep ())
      (snapshot [ bal "exchange:spot" "1.000000 USDC" ])
      ~now
  in
  print_endline
    (Risk_rules.Decision.to_string_hum (Outcome.to_decision skipped ~rule:"sweep"));
  [%expect
    {|
    (Dry_run Execute)
    allow
      sweep [allow] CUSTOM_TREASURY_SWEEP_EXCESS: excess above the threshold
    block
      sweep [block] CUSTOM_TREASURY_BELOW_THRESHOLD: balance at or below the threshold
    |}]
;;
