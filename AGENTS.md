# AGENTS.md: treasury-rules

## Purpose

treasury-rules evaluates an organisation's treasury policy (sweeps, top-ups, conversions,
rebalancing, yield limits, hedges) as pure functions over a snapshot of balances, prices and
recent runs, with daily caps, a drawdown pause and approval routing. It records each run once
per rule and period through a `Store` module type and keeps an append-only audit trail.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `treasury_rules` library: ids, rules, evaluation, outcomes, run records, runner, store, audit |
| `bin/` | The `treasury-rules` CLI: `check`, `evaluate`, `reason-codes` |
| `examples/` | `quick_start.ml` (the README example, output diffed in `runtest`) and example rules, settings and snapshot sexps |
| `test/` | Expect tests per area, Quickcheck properties, `fixtures.ml`, a cram test of the CLI |
| `.github/` | `workflows/ci.yml` |

## Build, test, docs

Run from the repository root, inside the workspace (`..` is the workspace root):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @treasury-rules/all
opam exec --switch 5.2.0+ox -- dune build --root .. @treasury-rules/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @treasury-rules/doc
```

For parallel work set `DUNE_BUILD_DIR=_build-treasury-rules` (resolved from the workspace
root because of `--root ..`). When a sibling's working tree fails only on warnings, add
`--profile release` to keep going; CI builds the pushed siblings.

Standalone: `opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`,
`opam install --deps-only .`, then `dune build @all @runtest`.

CLI: `dune exec --root .. ./treasury-rules/bin/main.exe -- evaluate -currency HYPE:8 -rules examples/rules.sexp -snapshot examples/snapshot.sexp -settings examples/settings.sexp`.

## Conventions

- Format only this repository: `dune build --root .. @treasury-rules/fmt --auto-promote`, after
  tests pass and `dune promote` is done. Never `dune fmt` across the workspace.
- `open! Core`, an `.mli` for every module, labelled arguments, odoc on every public value.
- Stable types: every persisted type has a `Stable.V1` whose bin_io digest is pinned in
  `test/test_stable.ml`. Add a `V2` instead of editing `V1`. `Rule`, `Outcome` and `Run_record`
  validate on read; keep that when adding fields.
- Times in records are `Time_ns.Alternate_sexp` (UTC sexp), so expect output is the same on
  every machine.
- Evaluation is pure: no clock, no I/O, no exceptions escaping (`Evaluate` turns them into
  `RULE_ERROR`). A new check needs its `Reason_code`, a place in the documented order, an
  expect test that shows the evidence, and where it holds an invariant, a Quickcheck property.
- Comments for humans at non-obvious choices (rounding direction, why a check comes first).
- `CHANGES.md` gets an "Unreleased" entry for user-visible changes.

## Dependencies and dependents

Depends on `money` (amounts, currencies, rates, rounding) and `risk-rules` (severity, hits,
decisions), plus `core`, `zarith`, `ppx_jane`. CI pins `fixed-decimal`, `money`, `ledger`,
`payments-core`, `risk-rules` in that order (risk-rules needs the middle two). No org repository
depends on it yet; services persist its records through their own `Store.S` implementation.

## CI

`.github/workflows/ci.yml` runs the infra `ox-setup` action with the pins above, `dune build`,
`dune test`, the examples' `runtest`, an odoc build that fails on warnings, and the `obj-magic`
reusable workflow. Check with `gh run list --limit 1` and at most one
`gh run watch <id> --exit-status`; never poll in a loop.

## Pitfalls

- A sibling repository mid-change (payments-core, risk-rules) breaks the workspace build of this
  one; the errors name the sibling's files. Filter for `treasury-rules/` and retry later.
- A functor parameter named `Store` or a local `module Runner = Runner.Make (...)` hides the
  library module of that name (`Store.Claim`, `Runner.Result`); alias first or pick another name.
- `Rule.t`, `Outcome.t` and `Run_record.t` are private: build them with their constructors and
  transitions, not record syntax.
- Amount comparisons with `Amount.( < )` order by currency first; compare only same-currency
  amounts, and convert through `Snapshot.rate` otherwise.
- A Quickcheck property can pass vacuously when every case skips; mutation-check it (break the
  rule, see it fail) before trusting it.
- zsh treats a word starting with `=` as a command path; do not `echo ====` as a separator.

## Where to read more

- `README.md` (rules table, check order, modes, exactly-once runs, CLI, known gaps).
- `../specs/repos/treasury-rules.md` when present.
- `../oxcaml-claude-skills/AGENTS.md` (workspace guide), the skills
  `../oxcaml-claude-skills/skills/banking-standards/` and `janestreet-ocaml-style/`, and
  `../oxcaml-claude-skills/GOTCHAS.md`.
- `../risk-rules/README.md` for the outcome shapes reused here.

## Never do

- Install opam packages that change the compiler, `core`, `base`, `async` or `ppx_jane` on the
  shared `5.2.0+ox` switch; dry-run first.
- Add I/O, a clock read or a database dependency to the library.
- Publish, tag or release.
- Commit credentials or real balances; examples are made-up values.
- Poll CI in a loop.
