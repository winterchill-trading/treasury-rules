# AGENTS.md: examples/

- `quick_start.ml`: the README example; `runtest` diffs its output with
  `quick_start.expected` (update both with the README).
- `rules.sexp`, `settings.sexp`, `snapshot.sexp`: inputs for the CLI and its cram test. They
  must parse with the current library; change them with any format change.
