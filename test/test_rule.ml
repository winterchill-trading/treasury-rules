open! Core
open! Treasury_rules
open Fixtures

let try_create kind =
  match Rule.create ~id:(id "r") ~name:"r" ~kind () with
  | Ok _ -> print_endline "ok"
  | Error e -> print_endline (Error.to_string_hum e)
;;

let%expect_test "validation" =
  try_create
    (Sweep
       { from = loc "a"; to_ = loc "a"; threshold = a "1.00 EUR"; minimum = a "0.00 EUR" });
  try_create
    (Sweep
       { from = loc "a"
       ; to_ = loc "b"
       ; threshold = a "1.00 EUR"
       ; minimum = a "1.000000 USDC"
       });
  try_create
    (Top_up
       { from = loc "a"
       ; to_ = loc "b"
       ; floor = a "10.00 EUR"
       ; target = a "5.00 EUR"
       ; minimum = a "0.00 EUR"
       });
  try_create
    (Convert
       { from = loc "a"
       ; to_ = loc "b"
       ; threshold = a "1.00 EUR"
       ; buy = eur
       ; minimum = a "0.00 EUR"
       ; max_per_run = None
       });
  try_create
    (Rebalance
       { targets =
           [ { location = loc "a"; asset = eur; weight_bps = 6_000 }
           ; { location = loc "b"; asset = eur; weight_bps = 3_000 }
           ]
       ; band_bps = 100
       ; minimum = a "0.000000 USDC"
       });
  try_create
    (Rebalance
       { targets =
           [ { location = loc "a"; asset = eur; weight_bps = 5_000 }
           ; { location = loc "a"; asset = eur; weight_bps = 5_000 }
           ]
       ; band_bps = 100
       ; minimum = a "0.000000 USDC"
       });
  try_create
    (Yield
       { cash = loc "a"
       ; fund = loc "b"
       ; buffer = a "1.00 EUR"
       ; max_share_bps = 12_000
       ; minimum = a "0.00 EUR"
       });
  try_create
    (Hedge
       { asset = hype
       ; staked = loc "a"
       ; position = loc "b"
       ; ratio_bps = 5_000
       ; band_bps = 500
       ; max_position = Some (a "1.000000 USDC")
       ; min_notional = a "0.000000 USDC"
       });
  [%expect
    {|
    (("invalid rule" (id r)) ("sweep: source and destination are the same" a))
    (("invalid rule" (id r))
     ("currencies differ: sweep" ("1.00 EUR" "1.000000 USDC")))
    (("invalid rule" (id r))
     ("top up target below floor" ((floor "10.00 EUR") (target "5.00 EUR"))))
    (("invalid rule" (id r)) ("convert buys the asset it sells" EUR))
    (("invalid rule" (id r)) ("rebalance weights do not sum to 10000" 9000))
    (("invalid rule" (id r)) ("rebalance legs repeat" ("a EUR" "a EUR")))
    (("invalid rule" (id r)) ("max_share out of range [0, 10000] bps" 12000))
    (("invalid rule" (id r))
     ("hedge max_position is not in the hedged asset" "1.000000 USDC"))
    |}]
;;

let sweep =
  rule
    "sweep"
    (Sweep
       { from = loc "a"; to_ = loc "b"; threshold = a "1.00 EUR"; minimum = a "0.00 EUR" })
;;

let%expect_test "status transitions and revisions" =
  let show r =
    match r with
    | Ok (r : Rule.t) -> print_s [%sexp (r.status : Rule.Status.t), (r.revision : int)]
    | Error e -> print_endline (Error.to_string_hum e)
  in
  show (Rule.pause sweep);
  show (Rule.resume sweep);
  show (Or_error.bind (Rule.pause sweep) ~f:Rule.resume);
  show (Or_error.bind (Rule.archive sweep) ~f:(fun r -> Rule.update r ~name:"x" ()));
  show (Rule.update sweep ~name:"" ());
  print_s [%sexp (Rule.is_scheduled sweep : bool)];
  print_s
    [%sexp
      (Rule.is_scheduled (Rule.update sweep ~mode:Dry_run () |> Or_error.ok_exn) : bool)];
  [%expect
    {|
    (Paused 2)
    ("rule status does not allow this change"
     ((id sweep) (status Active) (to_ Active)))
    (Active 3)
    ("rule is archived" sweep)
    (("invalid rule" (id sweep)) ("rule name must have 1 to 80 characters" ""))
    true
    false
    |}]
;;

let%expect_test "sexp form and validating reader" =
  let guards =
    { Rule.Guards.daily_cap = Some (a "50000.000000 USDC")
    ; approval_above = Some (a "10000.000000 USDC")
    ; drawdown =
        Some { asset = hype; max_drawdown_bps = 1_500; lookback = Time_ns.Span.of_day 7. }
    }
  in
  let r = Rule.update sweep ~guards ~cadence:Cadence.hourly () |> Or_error.ok_exn in
  let sexp = [%sexp (r : Rule.t)] in
  print_s sexp;
  print_s [%sexp (Rule.equal r (Rule.t_of_sexp sexp) : bool)];
  let bad =
    Sexp.of_string
      (String.substr_replace_all
         (Sexp.to_string sexp)
         ~pattern:"(revision 2)"
         ~with_:"(revision 0)")
  in
  print_s [%sexp (Or_error.try_with (fun () -> Rule.t_of_sexp bad) : Rule.t Or_error.t)];
  [%expect
    {|
    ((id sweep) (name sweep)
     (kind (Sweep (from a) (to_ b) (threshold "1.00 EUR") (minimum "0.00 EUR")))
     (guards
      ((daily_cap "50000.000000 USDC") (approval_above "10000.000000 USDC")
       (drawdown ((asset HYPE) (max_drawdown_bps 1500) (lookback 7d)))))
     (cadence 1h) (mode Live) (status Active) (revision 2))
    true
    (Error (("invalid rule" (id sweep)) ("revision below 1" 0)))
    |}]
;;
