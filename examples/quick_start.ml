(* The README quick start: a sweep rule run twice in one hour, once through the store. *)

open! Core
open Money
open Treasury_rules
module Runner = Runner.Make (Monad.Ident) (Store_memory)

let () =
  let now = Time_ns.of_string_with_utc_offset "2026-10-14T10:00:00Z" in
  let settings = Settings.default ~valuation:Currency.usdc in
  let rule =
    Rule.create
      ~id:(Rule_id.of_string "sweep-spot")
      ~name:"Sweep spot USDC above 50k"
      ~kind:
        (Sweep
           { from = Location.of_string "exchange:spot"
           ; to_ = Location.of_string "custody:cold"
           ; threshold = Amount.of_string "50000.000000 USDC"
           ; minimum = Amount.of_string "1000.000000 USDC"
           })
      ~cadence:Cadence.hourly
      ~mode:Live
      ()
    |> Or_error.ok_exn
  in
  let snapshot =
    Snapshot.create
      ~at:now
      ~balances:
        [ { location = Location.of_string "exchange:spot"
          ; amount = Amount.of_string "80000.000000 USDC"
          }
        ]
      ()
  in
  (* Pure: what would the rule do now? *)
  print_endline (Outcome.to_string_hum (Evaluate.evaluate settings rule snapshot ~now));
  (* Recorded: the second tick in the same hour finds the run already settled. *)
  let store = Store_memory.create () in
  Runner.create_rule store ~actor:(User "u-1") ~now rule |> Or_error.ok_exn;
  List.iter
    [ now; Time_ns.add now (Time_ns.Span.of_min 20.) ]
    ~f:(fun now ->
      List.iter (Runner.tick store settings snapshot ~now ~actor:System) ~f:(fun result ->
        match result with
        | Recorded r -> printf "recorded %s\n" (Run_key.to_string r.key)
        | Already r -> printf "already recorded %s\n" (Run_key.to_string r.key)
        | Not_scheduled _ -> ()))
;;
