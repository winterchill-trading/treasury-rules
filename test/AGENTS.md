# AGENTS.md: test/

Inline tests (ppx_expect and Quickcheck) and a cram test of the CLI.

- `fixtures.ml`: currencies (registers HYPE), locations, a fixed clock, snapshots, rules and
  `show`. Add helpers there.
- `test_evaluate_kinds.ml` (one test per rule kind), `test_guards.ml` (check order, caps,
  shadow, routing), `test_rule.ml`, `test_run_record.ml`, `test_runner.ml`,
  `test_store_conformance.ml`, `test_stable.ml` (pinned digests), `test_properties.ml`.
- `cli.t/run.t` runs the CLI on `../examples/*.sexp`.
- Promote with `dune promote --root ..` after reading the diff; a changed digest in
  `test_stable.ml` means a wire change, not a value to re-pin silently.
