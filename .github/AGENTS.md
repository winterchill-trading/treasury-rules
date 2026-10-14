# AGENTS.md: .github/

`workflows/ci.yml`, the only workflow.

- The `ox-setup` pins list sibling repositories in dependency order (fixed-decimal, money,
  ledger, payments-core, risk-rules); keep them in step with `dune-project`.
- Check runs with `gh run list --limit 1`, at most one `gh run watch <id> --exit-status`.
