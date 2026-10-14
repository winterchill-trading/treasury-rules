open! Core
open! Treasury_rules
open Fixtures

(* Pinned bin_prot shape digests. A change here means records written by an earlier
   version can no longer be read: add a V2 instead of editing V1. Rule, Outcome and
   Run_record embed money's stable types, so their digests also move when money's do. *)
let%expect_test "stable digests" =
  List.iter
    [ "Rule_id", [%bin_digest: Rule_id.Stable.V1.t]
    ; "Location", [%bin_digest: Location.Stable.V1.t]
    ; "Reason_code", [%bin_digest: Reason_code.Stable.V1.t]
    ; "Mode", [%bin_digest: Mode.Stable.V1.t]
    ; "Cadence", [%bin_digest: Cadence.Stable.V1.t]
    ; "Run_key", [%bin_digest: Run_key.Stable.V1.t]
    ; "Action", [%bin_digest: Action.Stable.V1.t]
    ; "Hit", [%bin_digest: Hit.Stable.V1.t]
    ; "Rule", [%bin_digest: Rule.Stable.V1.t]
    ; "Settings", [%bin_digest: Settings.Stable.V1.t]
    ; "Outcome", [%bin_digest: Outcome.Stable.V1.t]
    ; "Run_record", [%bin_digest: Run_record.Stable.V1.t]
    ; "Snapshot", [%bin_digest: Snapshot.Stable.V1.t]
    ; "Audit", [%bin_digest: Audit.Stable.V1.t]
    ]
    ~f:(fun (name, digest) -> printf "%-12s %s\n" name digest);
  [%expect
    {|
    Rule_id      d9a8da25d5656b016fb4dbdc2e4197fb
    Location     d9a8da25d5656b016fb4dbdc2e4197fb
    Reason_code  c08a2d3b2841395fa317fb723475ffd8
    Mode         f3d7fbe41fced42196ed9659242ece06
    Cadence      2b528f4b22f08e28876ffe0239315ac2
    Run_key      7d07978db12d0af2a0fb755c254640bd
    Action       ddeb9d9f7c5bf6a7d6e152612b189781
    Hit          9164c7998436da5a3d89c8f8f22c48d9
    Rule         c320f3a1ea64bdf054bbbb34b9042879
    Settings     99f59f7c55d2b4b67658b4b704090739
    Outcome      884bd5726decb15c3a1097453ed90775
    Run_record   d3385c89514fb7d0c75c0303fb0c369d
    Snapshot     c3781d4823d78f54c52ca6abaebf142a
    Audit        31c2c3c6fd0d12b3378ea547e7f795ca
    |}]
;;

let rule =
  rule
    ~guards:{ Rule.Guards.none with approval_above = Some (a "1000.000000 USDC") }
    "sweep"
    (Sweep
       { from = loc "exchange:spot"
       ; to_ = loc "bank:reserve"
       ; threshold = a "1000.000000 USDC"
       ; minimum = a "10.000000 USDC"
       })
;;

let record =
  let key = Run_key.create rule.id rule.cadence ~now in
  let outcome =
    Evaluate.evaluate
      settings
      rule
      (snapshot [ bal "exchange:spot" "1500.000000 USDC" ])
      ~now
  in
  Run_record.record (Run_record.claim key ~mode:Live ~now) outcome ~now |> Or_error.ok_exn
;;

let roundtrip (type a) (module M : Binable.S with type t = a) (v : a) =
  Binable.of_string (module M) (Binable.to_string (module M) v)
;;

let%expect_test "bin_io round trips and sexp forms" =
  print_s
    [%sexp
      (Rule.equal rule (roundtrip (module Rule.Stable.V1) rule) : bool)
      , (Run_record.equal record (roundtrip (module Run_record.Stable.V1) record) : bool)];
  print_s [%sexp (record : Run_record.Stable.V1.t)];
  let entry : Audit.t =
    { at = now
    ; actor = User "u-1"
    ; rule = rule.id
    ; event =
        Run_recorded
          { key = record.key; status = record.status; codes = [ Sweep_excess ] }
    }
  in
  print_s [%sexp (entry : Audit.Stable.V1.t)];
  print_s
    [%sexp
      (Reason_code.of_string "CUSTOM_MY_CHECK" : Reason_code.t)
      , (List.map Reason_code.builtin ~f:Reason_code.to_string : string list)];
  [%expect
    {|
    (true true)
    ((key ((rule sweep) (period_start "2026-10-14 00:00:00Z"))) (mode Live)
     (status Awaiting_execution) (attempt 1)
     (outcome
      ((rule sweep) (revision 1) (mode Live)
       (evaluated_at "2026-10-14 10:00:00Z") (route Execute)
       (actions
        (((kind
           (Transfer (from exchange:spot) (to_ bank:reserve)
            (amount "500.000000 USDC")))
          (reason SWEEP_EXCESS) (value "500.000000 USDC")
          (minimum "10.000000 USDC"))))
       (hits
        (((code SWEEP_EXCESS) (severity Allow)
          (detail "excess above the threshold")
          (evidence
           ((balance "1500.000000 USDC") (threshold "1000.000000 USDC")
            (excess "500.000000 USDC"))))))))
     (version 2) (created_at "2026-10-14 10:00:00Z")
     (updated_at "2026-10-14 10:00:00Z"))
    ((at "2026-10-14 10:00:00Z") (actor (User u-1)) (rule sweep)
     (event
      (Run_recorded (key ((rule sweep) (period_start "2026-10-14 00:00:00Z")))
       (status Awaiting_execution) (codes (SWEEP_EXCESS)))))
    (CUSTOM_MY_CHECK
     (SWEEP_EXCESS TOP_UP_FLOOR CONVERT_EXCESS REBALANCE_DRIFT YIELD_SUBSCRIBE
      YIELD_REDEEM_LIQUIDITY YIELD_REDEEM_OVER_LIMIT HEDGE_ADJUST RULE_INACTIVE
      PREVIOUS_PENDING NO_BALANCE SNAPSHOT_STALE PRICE_UNAVAILABLE
      BELOW_THRESHOLD BELOW_MINIMUM FLOOR_MET BELOW_BUFFER WITHIN_BAND
      ALLOCATION_LIMIT_REACHED INSUFFICIENT_BALANCE POSITION_LIMIT_REACHED
      RULE_DAILY_CAP_REACHED ORG_DAILY_CAP_REACHED DRAWDOWN_PAUSE
      DAILY_CAP_APPLIED APPROVAL_REQUIRED MANUAL_EXECUTION RULE_ERROR))
    |}]
;;

let%expect_test "every built-in code round-trips through its text form" =
  List.iter Reason_code.builtin ~f:(fun c ->
    assert (Reason_code.equal c (Reason_code.of_string (Reason_code.to_string c))));
  print_s
    [%sexp
      (List.map Reason_code.Group.all ~f:(fun g ->
         ( g
         , List.count Reason_code.builtin ~f:(fun c ->
             Reason_code.Group.equal (Reason_code.group c) g) ))
       : (Reason_code.Group.t * int) list)];
  [%expect {| ((Action 8) (Skip 17) (Route 3)) |}]
;;
