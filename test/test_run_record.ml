open! Core
open! Treasury_rules
open Fixtures

let key = Run_key.create (id "r") Cadence.daily ~now

let outcome route =
  let action : Action.t =
    { kind = Transfer { from = loc "a"; to_ = loc "b"; amount = a "5.00 EUR" }
    ; reason = Sweep_excess
    ; value = a "5.400000 USDC"
    ; minimum = a "0.00 EUR"
    }
  in
  let hits : Hit.t list =
    match route with
    | `Execute -> [ Hit.action Sweep_excess "x" [] ]
    | `Approve -> [ Hit.action Sweep_excess "x" []; Hit.review Approval_required "y" [] ]
    | `Skip -> [ Hit.block Below_threshold "z" [] ]
  in
  Outcome.create
    ~rule:(id "r")
    ~revision:1
    ~mode:Live
    ~evaluated_at:now
    ~actions:[ action ]
    ~hits
;;

let status = function
  | Ok (r : Run_record.t) ->
    print_s [%sexp (r.status : Run_record.Status.t), (r.version : int), (r.attempt : int)]
  | Error e -> print_endline (Error.to_string_hum e)
;;

let%expect_test "record routes by mode and outcome" =
  let claim mode = Run_record.claim key ~mode ~now in
  List.iter
    [ Mode.Live, `Execute; Live, `Approve; Live, `Skip; Shadow, `Execute; Shadow, `Skip ]
    ~f:(fun (mode, route) -> status (Run_record.record (claim mode) (outcome route) ~now));
  let other =
    Outcome.create
      ~rule:(id "other")
      ~revision:1
      ~mode:Live
      ~evaluated_at:now
      ~actions:[]
      ~hits:[]
  in
  status (Run_record.record (claim Live) other ~now);
  [%expect
    {|
    (Awaiting_execution 2 1)
    (Awaiting_approval 2 1)
    (Skipped 2 1)
    (Shadowed 2 1)
    (Skipped 2 1)
    ("outcome is for another rule" (run r:2026-10-14T00:00:00Z) (outcome other))
    |}]
;;

let%expect_test "transitions refuse the wrong state" =
  let ok = Or_error.ok_exn in
  let claimed = Run_record.claim key ~mode:Live ~now in
  status (Run_record.approve claimed ~now);
  status (Run_record.executed claimed ~now);
  let approving = Run_record.record claimed (outcome `Approve) ~now |> ok in
  status (Run_record.executed approving ~now);
  status (Run_record.take_over approving ~now);
  let rejected = Run_record.reject approving ~note:"n" ~now |> ok in
  status (Ok rejected);
  status (Run_record.approve rejected ~now);
  status (Run_record.fail claimed ~reason:"raised" ~now);
  let later = Time_ns.add now (Time_ns.Span.of_min 3.) in
  print_s
    [%sexp
      (Run_record.is_stale claimed ~now:later ~stale_after:(Time_ns.Span.of_min 2.)
       : bool)
      , (Run_record.is_stale approving ~now:later ~stale_after:(Time_ns.Span.of_min 2.)
         : bool)];
  status (Run_record.take_over claimed ~now:later);
  [%expect
    {|
    ("run status does not allow this change" approve (run r:2026-10-14T00:00:00Z)
     (status Evaluating))
    ("run status does not allow this change" "mark executed"
     (run r:2026-10-14T00:00:00Z) (status Evaluating))
    ("run status does not allow this change" "mark executed"
     (run r:2026-10-14T00:00:00Z) (status Awaiting_approval))
    ("run status does not allow this change" "take over"
     (run r:2026-10-14T00:00:00Z) (status Awaiting_approval))
    (Rejected 3 1)
    ("run status does not allow this change" approve (run r:2026-10-14T00:00:00Z)
     (status Rejected))
    (Failed 2 1)
    (true false)
    (Evaluating 2 2)
    |}]
;;

let%expect_test "reading checks consistency" =
  let r =
    Run_record.record (Run_record.claim key ~mode:Live ~now) (outcome `Execute) ~now
    |> Or_error.ok_exn
  in
  let s = Sexp.to_string [%sexp (r : Run_record.t)] in
  let read s =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> Run_record.t_of_sexp (Sexp.of_string s))
         |> Or_error.map ~f:(fun (r : Run_record.t) -> r.status)
         : Run_record.Status.t Or_error.t)]
  in
  read s;
  read
    (String.substr_replace_first
       s
       ~pattern:"(status Awaiting_execution)"
       ~with_:"(status Evaluating)");
  [%expect
    {|
    (Ok Awaiting_execution)
    (Error ("inconsistent run record" "evaluating run has an outcome"))
    |}]
;;
