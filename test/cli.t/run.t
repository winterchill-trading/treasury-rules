Check the example rules file.

  $ treasury-rules check -currency HYPE:8 -rules ../../examples/rules.sexp
  sweep-spot: sweep, live, every 1h, Active
  payroll-runway: convert, live, every 1d, Active
  idle-yield: yield, shadow, every 1d, Active
  stake-hedge: hedge, live, every 1h, Active

Evaluate it against the example snapshot.

  $ treasury-rules evaluate -currency HYPE:8 -rules ../../examples/rules.sexp -snapshot ../../examples/snapshot.sexp -settings ../../examples/settings.sexp -evidence
  sweep-spot r1 (live): approve
    transfer 130000.000000 USDC exchange:spot -> custody:cold
    [allow] SWEEP_EXCESS: excess above the threshold
    [review] APPROVAL_REQUIRED: an action is above the approval threshold
      balance = 180000.000000 USDC
      threshold = 50000.000000 USDC
      excess = 130000.000000 USDC
      threshold = 100000.000000 USDC
      largest = 130000.000000 USDC
      count = 1
  payroll-runway r1 (live): execute
    convert 30000.000000 USDC exchange:spot -> ~27649.76 EUR bank:eur at 200/217
    [allow] CONVERT_EXCESS: excess above the threshold is converted
      balance = 180000.000000 USDC
      threshold = 20000.000000 USDC
      sell = 30000.000000 USDC
      rate = 200/217
      buy = 27649.76 EUR
  idle-yield r1 (shadow): execute
    subscribe 20000.000000 USDC custody:cold -> fund:tbill
    [allow] YIELD_SUBSCRIBE: cash above the buffer
      cash = 120000.000000 USDC
      fund = 40000.000000 USDC
      buffer = 100000.000000 USDC
      limit = 80000.000000 USDC
      amount = 20000.000000 USDC
  stake-hedge r1 (live): execute
    hedge sell 1000.00000000 HYPE perp:HYPE at 31.2
    [allow] HEDGE_ADJUST: the hedge drifted outside its band
      staked = 8000.00000000 HYPE
      position = -3000.00000000 HYPE
      target = -4000.00000000 HYPE
      drift = -1000.00000000 HYPE
      band = 400.00000000 HYPE
      price = 31.2
      notional = 31200.000000 USDC

An hour later the snapshot is too old to act on.

  $ treasury-rules evaluate -currency HYPE:8 -rules ../../examples/rules.sexp -snapshot ../../examples/snapshot.sexp -settings ../../examples/settings.sexp -now 2026-10-14T11:00:00Z
  sweep-spot r1 (live): skip
    [block] SNAPSHOT_STALE: balances are older than allowed
  payroll-runway r1 (live): skip
    [block] SNAPSHOT_STALE: balances are older than allowed
  idle-yield r1 (shadow): skip
    [block] SNAPSHOT_STALE: balances are older than allowed
  stake-hedge r1 (live): skip
    [block] SNAPSHOT_STALE: balances are older than allowed

A file that refers to an unregistered token fails with exit code 2.

  $ treasury-rules check -rules ../../examples/rules.sexp 2>&1 | head -c 60; echo
  (Of_sexp_error ../../examples/rules.sexp:15:22 "(\"unknown c
  $ treasury-rules check -rules ../../examples/rules.sexp > /dev/null 2>&1
  [2]

  $ treasury-rules reason-codes | head -3
  SWEEP_EXCESS               Action balance above the threshold is moved to the destination
  TOP_UP_FLOOR               Action balance below the floor is refilled from the source
  CONVERT_EXCESS             Action balance above the threshold is converted into another asset
