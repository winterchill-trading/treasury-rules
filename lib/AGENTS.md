# AGENTS.md: lib/

The `treasury_rules` library. Each module has an `.mli`; change it first and keep its odoc.

- `id.ml`, `rule_id.ml`, `location.ml`: validated string ids (validated on read too).
- `reason_code.ml`, `hit.ml`, `outcome.ml`: findings and the route derived from them, in
  risk-rules' shape (`Outcome.to_decision`).
- `rule.ml`, `cadence.ml`, `mode.ml`, `settings.ml`: what a rule is and its validation.
- `snapshot.ml`, `action.ml`, `evaluate.ml`: the pure evaluation. The order of checks in
  `evaluate.ml` is documented in `evaluate.mli` and the README; keep all three in step.
- `run_key.ml`, `run_record.ml`: one run per rule and period and its state machine.
- `store.ml`, `store_memory.ml`, `store_conformance.ml`, `runner.ml`, `audit.ml`: persistence
  interface, in-memory store, its conformance suite and the runner.

Amounts round down when the library proposes moving money (never propose more than exists).
