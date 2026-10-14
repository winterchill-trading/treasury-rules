# treasury-rules

Policy rules for an organisation's treasury: sweep cash above a threshold, top up a
balance below a floor, convert an asset when there is too much of it, rebalance to target
allocations, keep yield or staking within limits, and hedge a staked position at a ratio.
Each rule is a pure function of a snapshot (balances, prices, recent runs) that proposes
actions with reason codes and evidence. Guards add daily caps per rule and across rules,
a pause on a drawdown of a reference price, freshness checks and approval thresholds.
Runs are recorded once per rule and period, and every change lands in an audit trail.

Requires the OxCaml compiler (`5.2.0+ox`).

## Why separate

Treasury automation is policy, not fraud control: the question is "what should move,
and how much", not "should this payment be stopped". Keeping it apart from
[risk-rules](https://github.com/winterchill-trading/risk-rules) keeps both small, while
reusing its outcome shapes (severity, reason codes, hits with evidence) so the same
audit and review tooling can show both. Keeping it apart from any service means the
policy can be evaluated in a test, a shell or a back-test with no database: the library
never reads a clock, a ledger or a price feed on its own.

## Quick start

```ocaml
open! Core
open Money
open Treasury_rules
module Runner = Runner.Make (Monad.Ident) (Store_memory)

let now = Time_ns.of_string_with_utc_offset "2026-10-14T10:00:00Z"
let settings = Settings.default ~valuation:Currency.usdc

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

let snapshot =
  Snapshot.create
    ~at:now
    ~balances:
      [ { location = Location.of_string "exchange:spot"
        ; amount = Amount.of_string "80000.000000 USDC"
        }
      ]
    ()

(* Pure: what would the rule do now? *)
let () = print_endline (Outcome.to_string_hum (Evaluate.evaluate settings rule snapshot ~now))
(* sweep-spot r1 (live): execute
     transfer 30000.000000 USDC exchange:spot -> custody:cold
     [allow] SWEEP_EXCESS: excess above the threshold *)

(* Recorded: a second tick in the same hour finds the run already recorded. *)
let () =
  let store = Store_memory.create () in
  Runner.create_rule store ~actor:(User "u-1") ~now rule |> Or_error.ok_exn;
  ignore (Runner.tick store settings snapshot ~now ~actor:System : Runner.Result.t list)
```

The complete program is `examples/quick_start.ml`; CI runs it and compares its output
with `examples/quick_start.expected`.

## Rules

| Kind | Proposes | Skips with |
| --- | --- | --- |
| `Sweep` | a transfer of everything above `threshold` | `BELOW_THRESHOLD`, `BELOW_MINIMUM` |
| `Top_up` | a transfer that refills a balance below `floor` up to `target`, limited by the source | `FLOOR_MET`, `INSUFFICIENT_BALANCE`, `BELOW_MINIMUM` |
| `Convert` | a conversion of the excess above `threshold` into `buy`, capped by `max_per_run`, with an estimate at the fresh rate rounded down | `BELOW_THRESHOLD`, `BELOW_MINIMUM`, `PRICE_UNAVAILABLE` |
| `Rebalance` | transfers or conversions from overweight to underweight legs, back to the exact weights, once any leg drifts outside `band_bps` | `WITHIN_BAND`, `BELOW_MINIMUM` |
| `Yield` | a subscription of cash above `buffer` up to `max_share_bps` of cash + fund; a redemption when cash falls below the buffer or the share is exceeded | `BELOW_BUFFER`, `ALLOCATION_LIMIT_REACHED`, `BELOW_MINIMUM` |
| `Hedge` | an order that brings a short position to `ratio_bps` of the staked amount, never beyond `max_position` | `WITHIN_BAND`, `POSITION_LIMIT_REACHED`, `BELOW_MINIMUM` |

Every rule also has guards, a cadence (periods aligned to the epoch in UTC) and a mode.
`Evaluate.evaluate` checks, in this order, and the first check that fails is the only
`Block` hit of the outcome:

1. the rule is active (`RULE_INACTIVE`);
2. the snapshot is fresh (`SNAPSHOT_STALE`);
3. no earlier run of the rule is awaiting approval or execution (`PREVIOUS_PENDING`);
4. the drawdown guard: the reference price is not more than `max_drawdown_bps` below
   its peak over `lookback` (`DRAWDOWN_PAUSE`);
5. the kind's own logic (table above), with fresh prices only (`PRICE_UNAVAILABLE`);
6. the rule's daily cap, then the organisation's, over today's (UTC) committed runs:
   actions are cut to what is left (`DAILY_CAP_APPLIED`) or the run stops
   (`RULE_DAILY_CAP_REACHED`, `ORG_DAILY_CAP_REACHED`);
7. routing: `MANUAL_EXECUTION` when automatic execution is off, `APPROVAL_REQUIRED`
   when an action's value is above the rule's threshold.

The outcome's route is derived from its hits, like risk-rules' decisions: any `Block`
hit (or no action) is `Skip`, any `Review` hit is `Approve`, otherwise `Execute`.
`Outcome.to_decision` gives the same outcome as a `Risk_rules.Decision.t`.

## Modes

- `Live`: runs are recorded, and actions go to approval or to execution.
- `Shadow`: runs are recorded with their actions but nothing executes, and nothing
  counts against the organisation's cap. The rule's own cap counts its own shadow runs,
  so a shadow rule shows what the live rule would have done.
- `Dry_run`: the scheduler skips the rule; `Runner.preview` (or `Evaluate.evaluate
  ~mode:Dry_run`) evaluates any rule on demand and records nothing. New rules start
  here.

## Runs, exactly once

A run is keyed by rule and period (`Run_key`). The runner claims the key in the store
before evaluating; a second tick in the same period gets the existing record back. A
claim still `Evaluating` after `stale_claim_after` belonged to a runner that crashed and
is taken over under the same key, and the late writer loses its compare-and-set. Nothing
executes while a run is `Evaluating`, so a take-over never doubles an action, and the
action keys (`Run_key.action_key`) are stable across attempts for use as idempotency keys
with the rails.

```
Evaluating -> Skipped | Shadowed | Awaiting_approval | Awaiting_execution | Failed
Awaiting_approval -> Awaiting_execution | Rejected
Awaiting_execution -> Executed | Failed
```

The library never executes anything. A service reads runs awaiting execution, carries
out their actions, and reports `Runner.executed` or `Runner.failed`.

## Storage and audit

`Store.S` is a module type over any monad: rules with compare-and-set on their revision,
run records with an atomic claim and compare-and-set on their version, recent runs, and
an append-only audit trail (`Audit.t`: who, when, which rule, what happened).
`Store_memory` implements it for tests and offline use, and `Store_conformance` is a
suite a database implementation runs in its own tests.

## Command line

```sh
treasury-rules check -currency HYPE:8 -rules examples/rules.sexp
treasury-rules evaluate -currency HYPE:8 -rules examples/rules.sexp \
  -snapshot examples/snapshot.sexp -settings examples/settings.sexp -evidence
treasury-rules reason-codes
```

`-currency CODE:DIGITS` registers a token money does not know. Errors exit with code 2.

## Stable types

`Rule_id`, `Location`, `Reason_code`, `Mode`, `Cadence`, `Run_key`, `Action`, `Hit`,
`Rule`, `Settings`, `Outcome`, `Run_record`, `Snapshot` and `Audit` have `Stable.V1`
bin_io and sexp forms whose digests are pinned in `test/test_stable.ml`. Readers of
`Rule`, `Outcome` and `Run_record` validate: a stored rule that fails validation, an
outcome whose route disagrees with its hits, or a run record whose status disagrees with
its outcome is refused on read.

## Known gaps

- Caps across rules hold when one runner at a time ticks an organisation; two concurrent
  runners can each miss the other's run of the same instant.
- Days for caps are UTC days, and a run counts on the day its period starts.
- Execution status is per run, not per action; a partial failure is a failed run.
- Conversion and rebalance estimates ignore fees and slippage; hedges do not size margin.
- A pending run blocks later runs of the same rule; there is no "superseded" state.

## Docs

- `AGENTS.md` for working on this repository.
- API documentation: `dune build @doc`.

## License

MIT
