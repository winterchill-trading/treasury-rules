(lang dune 3.17)

(name treasury-rules)

(generate_opam_files true)

(license MIT)

(authors "WinterChill AI")

(maintainers "hello@winterchill.xyz")

(source
 (github winterchill-trading/treasury-rules))

(homepage "https://github.com/winterchill-trading/treasury-rules")

(bug_reports "https://github.com/winterchill-trading/treasury-rules/issues")

(documentation "https://github.com/winterchill-trading/treasury-rules#readme")

(package
 (name treasury-rules)
 (synopsis
  "Treasury policy rules: sweeps, conversions, rebalancing, yield limits and hedges")
 (description
  "treasury-rules evaluates an organisation's treasury policy. Each rule (sweep the excess above a threshold, top up a balance below a floor, convert one asset into another, rebalance to target allocations within a band, cap the share placed in yield or staking, hedge a staked position at a ratio) is a pure function of a snapshot of balances, prices and recent runs, and proposes actions with reason codes and evidence. Guards apply daily caps per rule and across rules, pause on a drawdown of a reference price, refuse stale prices and route large actions to approval. Rules run in live, shadow or dry-run mode; every run is recorded once per rule and period, with stale claims taken over under the same key, and every change is written to an append-only audit trail. Persistence is a Store module type with an in-memory implementation. Requires the OxCaml compiler.")
 (tags
  (treasury policy rules sweep rebalance hedge oxcaml))
 (depends
  oxcaml
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3~)))
  dune
  (core
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  zarith
  money
  risk-rules
  (core_unix
   (and
    :with-test
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (base_quickcheck :with-test)
  (odoc :with-doc)))

(package
 (name treasury-rules-cli)
 (synopsis "Command-line tool that evaluates treasury rules against a snapshot")
 (description
  "The treasury-rules command checks rule files and evaluates rules against a snapshot of balances, prices and recent runs, printing the proposed actions and the reasons. Built on the treasury-rules library.")
 (tags
  (treasury policy cli oxcaml))
 (depends
  oxcaml
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3~)))
  dune
  (treasury-rules
   (= :version))
  (core
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (core_unix
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (odoc :with-doc)))
