open! Core
open Treasury_rules
open Fixtures
module R = Runner.Make (Monad.Ident) (Store_memory)

let sweep ?(name = "sweep") ?guards ?(mode = Mode.Live) () =
  rule
    ?guards
    ~mode
    ~cadence:Cadence.hourly
    name
    (Sweep
       { from = loc "exchange:spot"
       ; to_ = loc "bank:reserve"
       ; threshold = a "1000.000000 USDC"
       ; minimum = a "10.000000 USDC"
       })
;;

let snap ?(at = now) cash = snapshot ~at [ bal "exchange:spot" cash ]

let show_result (r : Runner.Result.t) =
  match r with
  | Recorded r ->
    printf
      "recorded %s %s\n"
      (Run_key.to_string r.key)
      (Sexp.to_string [%sexp (r.status : Run_record.Status.t)])
  | Already r ->
    printf
      "already %s %s\n"
      (Run_key.to_string r.key)
      (Sexp.to_string [%sexp (r.status : Run_record.Status.t)])
  | Not_scheduled id -> printf "not scheduled %s\n" (Rule_id.to_string id)
;;

let show_audit store =
  List.iter (Store_memory.audit store ~rule:None) ~f:(fun e ->
    print_endline (Audit.to_string_hum e))
;;

let ok = Or_error.ok_exn
let user = Audit.Actor.User "u-1"

let%expect_test "one run per rule and period, however often the scheduler ticks" =
  let store = Store_memory.create () in
  R.create_rule store ~actor:user ~now (sweep ()) |> ok;
  R.create_rule store ~actor:user ~now (sweep ~name:"draft" ~mode:Dry_run ()) |> ok;
  let tick at =
    R.tick store settings (snap ~at "6000.000000 USDC") ~now:at ~actor:System
  in
  List.iter (tick now) ~f:show_result;
  List.iter (tick (at "2026-10-14T10:59:00Z")) ~f:show_result;
  List.iter (tick (at "2026-10-14T11:00:00Z")) ~f:show_result;
  show_audit store;
  [%expect
    {|
    not scheduled draft
    recorded sweep:2026-10-14T10:00:00Z Awaiting_execution
    not scheduled draft
    already sweep:2026-10-14T10:00:00Z Awaiting_execution
    not scheduled draft
    recorded sweep:2026-10-14T11:00:00Z Skipped
    2026-10-14T10:00:00.000000000Z user u-1 sweep: created (sweep, live)
    2026-10-14T10:00:00.000000000Z user u-1 draft: created (sweep, dry_run)
    2026-10-14T10:00:00.000000000Z system sweep: claimed sweep:2026-10-14T10:00:00Z (attempt 1)
    2026-10-14T10:00:00.000000000Z system sweep: recorded sweep:2026-10-14T10:00:00Z: Awaiting_execution [SWEEP_EXCESS]
    2026-10-14T11:00:00.000000000Z system sweep: claimed sweep:2026-10-14T11:00:00Z (attempt 1)
    2026-10-14T11:00:00.000000000Z system sweep: recorded sweep:2026-10-14T11:00:00Z: Skipped [PREVIOUS_PENDING]
    |}]
;;

let%expect_test "approval, rejection, execution and failure are audited" =
  let store = Store_memory.create () in
  let guards = { Rule.Guards.none with approval_above = Some (a "1000.000000 USDC") } in
  R.create_rule store ~actor:user ~now (sweep ~guards ()) |> ok;
  let run at =
    R.tick store settings (snap ~at "6000.000000 USDC") ~now:at ~actor:System
  in
  List.iter (run now) ~f:show_result;
  let key = Run_key.create (id "sweep") Cadence.hourly ~now in
  (* Pending: the next period waits. *)
  let later = at "2026-10-14T11:05:00Z" in
  List.iter (run later) ~f:show_result;
  let r = R.approve store ~actor:(User "approver") ~now:later key |> ok in
  print_s [%sexp (r.status : Run_record.Status.t)];
  (match R.reject store ~actor:user ~now:later ~note:"no" key with
   | Ok _ -> print_endline "rejected an approved run"
   | Error e -> print_endline (Error.to_string_hum e));
  let r = R.executed store ~actor:System ~now:later key |> ok in
  print_s [%sexp (r.status : Run_record.Status.t)];
  let next = at "2026-10-14T12:05:00Z" in
  List.iter (run next) ~f:show_result;
  let k2 = Run_key.create (id "sweep") Cadence.hourly ~now:next in
  let r = R.reject store ~actor:user ~now:next ~note:"too big" k2 |> ok in
  print_s [%sexp (r.status : Run_record.Status.t)];
  show_audit store;
  [%expect
    {|
    recorded sweep:2026-10-14T10:00:00Z Awaiting_approval
    recorded sweep:2026-10-14T11:00:00Z Skipped
    Awaiting_execution
    ("run status does not allow this change" reject
     (run sweep:2026-10-14T10:00:00Z) (status Awaiting_execution))
    Executed
    recorded sweep:2026-10-14T12:00:00Z Awaiting_approval
    Rejected
    2026-10-14T10:00:00.000000000Z user u-1 sweep: created (sweep, live)
    2026-10-14T10:00:00.000000000Z system sweep: claimed sweep:2026-10-14T10:00:00Z (attempt 1)
    2026-10-14T10:00:00.000000000Z system sweep: recorded sweep:2026-10-14T10:00:00Z: Awaiting_approval [SWEEP_EXCESS APPROVAL_REQUIRED]
    2026-10-14T11:05:00.000000000Z system sweep: claimed sweep:2026-10-14T11:00:00Z (attempt 1)
    2026-10-14T11:05:00.000000000Z system sweep: recorded sweep:2026-10-14T11:00:00Z: Skipped [PREVIOUS_PENDING]
    2026-10-14T11:05:00.000000000Z user approver sweep: approved sweep:2026-10-14T10:00:00Z
    2026-10-14T11:05:00.000000000Z system sweep: executed sweep:2026-10-14T10:00:00Z
    2026-10-14T12:05:00.000000000Z system sweep: claimed sweep:2026-10-14T12:00:00Z (attempt 1)
    2026-10-14T12:05:00.000000000Z system sweep: recorded sweep:2026-10-14T12:00:00Z: Awaiting_approval [SWEEP_EXCESS APPROVAL_REQUIRED]
    2026-10-14T12:05:00.000000000Z user u-1 sweep: rejected sweep:2026-10-14T12:00:00Z: too big
    |}]
;;

let%expect_test "a stale claim is taken over under the same key" =
  let store = Store_memory.create () in
  let rule = sweep () in
  R.create_rule store ~actor:user ~now rule |> ok;
  let key = Run_key.create rule.id rule.cadence ~now in
  (* A runner claims and crashes before recording. *)
  (match
     Store_memory.claim_run
       store
       key
       ~mode:Live
       ~now
       ~stale_after:settings.stale_claim_after
   with
   | Claimed _ -> ()
   | _ -> assert false);
  let soon = Time_ns.add now (Time_ns.Span.of_min 1.) in
  List.iter
    (R.tick store settings (snap ~at:soon "6000.000000 USDC") ~now:soon ~actor:System)
    ~f:show_result;
  let late = Time_ns.add now (Time_ns.Span.of_min 3.) in
  List.iter
    (R.tick store settings (snap ~at:late "6000.000000 USDC") ~now:late ~actor:System)
    ~f:show_result;
  let r = Store_memory.find_run store key |> Option.value_exn in
  printf
    "attempt %d version %d actions %s\n"
    r.attempt
    r.version
    (String.concat
       ~sep:", "
       (List.mapi (Run_record.actions r) ~f:(fun i _ -> Run_key.action_key key ~index:i)));
  [%expect
    {|
    already sweep:2026-10-14T10:00:00Z Evaluating
    recorded sweep:2026-10-14T10:00:00Z Awaiting_execution
    attempt 2 version 3 actions sweep:2026-10-14T10:00:00Z:0
    |}]
;;

let%expect_test "daily caps hold across rules within one tick" =
  let store = Store_memory.create () in
  let settings = { settings with org_daily_cap = Some (a "7000.000000 USDC") } in
  R.create_rule store ~actor:user ~now (sweep ~name:"a-sweep" ()) |> ok;
  R.create_rule store ~actor:user ~now (sweep ~name:"b-sweep" ()) |> ok;
  R.create_rule store ~actor:user ~now (sweep ~name:"c-sweep" ()) |> ok;
  let results = R.tick store settings (snap "6000.000000 USDC") ~now ~actor:System in
  List.iter results ~f:(function
    | Recorded r -> print_endline (Outcome.to_string_hum (Option.value_exn r.outcome))
    | other -> show_result other);
  [%expect
    {|
    a-sweep r1 (live): execute
      transfer 5000.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
    b-sweep r1 (live): execute
      transfer 2000.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
      [allow] DAILY_CAP_APPLIED: actions reduced to fit the daily cap
    c-sweep r1 (live): skip
      [block] ORG_DAILY_CAP_REACHED: the daily cap is used up
    |}]
;;

let%expect_test "rule changes are compare-and-set and audited" =
  let store = Store_memory.create () in
  R.create_rule store ~actor:user ~now (sweep ()) |> ok;
  print_s [%sexp (R.create_rule store ~actor:user ~now (sweep ()) : unit Or_error.t)];
  let change f = R.change_rule store ~actor:user ~now (id "sweep") ~f in
  ignore (change Rule.pause |> ok : Rule.t);
  ignore (change Rule.resume |> ok : Rule.t);
  ignore (change (fun r -> Rule.update r ~mode:Shadow ()) |> ok : Rule.t);
  ignore (change Rule.archive |> ok : Rule.t);
  print_s [%sexp (change Rule.resume : Rule.t Or_error.t)];
  let preview =
    R.preview
      store
      settings
      (snap "6000.000000 USDC")
      ~now
      (Store_memory.find_rule store (id "sweep") |> Option.value_exn)
  in
  print_endline (Outcome.to_string_hum preview);
  show_audit store;
  [%expect
    {|
    (Error ("rule exists" (id sweep)))
    (Error
     ("rule status does not allow this change"
      ((id sweep) (status Archived) (to_ Active))))
    sweep r5 (dry_run): skip
      [block] RULE_INACTIVE: the rule is not active
    2026-10-14T10:00:00.000000000Z user u-1 sweep: created (sweep, live)
    2026-10-14T10:00:00.000000000Z user u-1 sweep: Active -> Paused (r2)
    2026-10-14T10:00:00.000000000Z user u-1 sweep: Paused -> Active (r3)
    2026-10-14T10:00:00.000000000Z user u-1 sweep: updated to r4 (shadow)
    2026-10-14T10:00:00.000000000Z user u-1 sweep: Active -> Archived (r5)
    |}]
;;
