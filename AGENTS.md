# AGENTS.md: treasury-rules

## Purpose

treasury-rules evaluates an organisation's treasury policy (sweeps, conversions, rebalancing,
yield limits, hedges) as pure functions over a snapshot of balances, prices and recent runs. It
records each run once per rule and period and keeps an append-only audit trail through a `Store`
module type.

## Build, test, docs

Run from the repository root, inside the workspace (`..` is the workspace root):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @treasury-rules/all
opam exec --switch 5.2.0+ox -- dune build --root .. @treasury-rules/runtest
```

For parallel work, set `DUNE_BUILD_DIR=_build-treasury-rules`.

See `../oxcaml-claude-skills/AGENTS.md` for the workspace-wide guide.
