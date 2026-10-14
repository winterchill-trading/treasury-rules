# Changes

## Unreleased

## 0.1.0

- Rules: sweep, top-up, convert, rebalance, yield and hedge, with validation and an
  active, paused, archived lifecycle.
- Guards: daily caps per rule and per organisation across runs, drawdown pause on a
  reference price, price and snapshot freshness, pending-run check, approval thresholds
  and manual execution.
- Pure `Evaluate.evaluate` producing an `Outcome` with actions, hits and reason codes,
  convertible to a risk-rules decision.
- Live, shadow and dry-run modes.
- `Run_record` state machine with one run per rule and period and take-over of stale
  claims; `Runner` over a `Store.S` module type, `Store_memory` and `Store_conformance`.
- Append-only `Audit` trail.
- `treasury-rules` command: `check`, `evaluate`, `reason-codes`.
