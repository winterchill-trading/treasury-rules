# AGENTS.md: bin/

The `treasury-rules` command (`main.ml`, package `treasury-rules-cli`): `check`, `evaluate`,
`reason-codes`.

- Reads sexp files only; no network, no environment variables, records nothing.
- Exit code 2 on usage or parse errors. `-currency CODE:DIGITS` registers tokens before reading.
- `test/cli.t/run.t` pins the output; update it in the same commit as a format change.
