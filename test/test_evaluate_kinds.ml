open! Core
open! Treasury_rules
open Fixtures

let sweep =
  rule
    "sweep-usdc"
    (Sweep
       { from = loc "exchange:spot"
       ; to_ = loc "bank:reserve"
       ; threshold = a "10000.000000 USDC"
       ; minimum = a "100.000000 USDC"
       })
;;

let%expect_test "sweep moves the excess above the threshold" =
  show sweep (snapshot [ bal "exchange:spot" "12500.000000 USDC" ]);
  [%expect
    {|
    sweep-usdc r1 (live): execute
      transfer 2500.000000 USDC exchange:spot -> bank:reserve
      [allow] SWEEP_EXCESS: excess above the threshold
        balance = 12500.000000 USDC
        threshold = 10000.000000 USDC
        excess = 2500.000000 USDC
    |}]
;;

let%expect_test "sweep skips below the threshold and below the minimum" =
  show sweep (snapshot [ bal "exchange:spot" "9000.000000 USDC" ]);
  show sweep (snapshot [ bal "exchange:spot" "10050.000000 USDC" ]);
  show sweep (snapshot [ bal "bank:reserve" "1.000000 USDC" ]);
  [%expect
    {|
    sweep-usdc r1 (live): skip
      [block] BELOW_THRESHOLD: balance at or below the threshold
        balance = 9000.000000 USDC
        threshold = 10000.000000 USDC
    sweep-usdc r1 (live): skip
      [block] BELOW_MINIMUM: excess below the minimum
        balance = 10050.000000 USDC
        threshold = 10000.000000 USDC
        excess = 50.000000 USDC
        minimum = 100.000000 USDC
    sweep-usdc r1 (live): skip
      [block] NO_BALANCE: no USDC balance at exchange:spot
        location = exchange:spot
        asset = USDC
    |}]
;;

let top_up =
  rule
    "card-float"
    (Top_up
       { from = loc "bank:main"
       ; to_ = loc "card:float"
       ; floor = a "2000.00 EUR"
       ; target = a "5000.00 EUR"
       ; minimum = a "50.00 EUR"
       })
;;

let eur_prices = [ price "EUR" "USDC" "1.08" ]

let%expect_test "top-up refills to the target, limited by the source" =
  show
    top_up
    (snapshot
       ~prices:eur_prices
       [ bal "bank:main" "10000.00 EUR"; bal "card:float" "1500.00 EUR" ]);
  show
    top_up
    (snapshot
       ~prices:eur_prices
       [ bal "bank:main" "1000.00 EUR"; bal "card:float" "1500.00 EUR" ]);
  show
    top_up
    (snapshot
       ~prices:eur_prices
       [ bal "bank:main" "10.00 EUR"; bal "card:float" "1500.00 EUR" ]);
  show
    top_up
    (snapshot
       ~prices:eur_prices
       [ bal "bank:main" "10.00 EUR"; bal "card:float" "2000.00 EUR" ]);
  [%expect
    {|
    card-float r1 (live): execute
      transfer 3500.00 EUR bank:main -> card:float
      [allow] TOP_UP_FLOOR: balance below the floor
        balance = 1500.00 EUR
        floor = 2000.00 EUR
        target = 5000.00 EUR
        need = 3500.00 EUR
        source = 10000.00 EUR
        amount = 3500.00 EUR
    card-float r1 (live): execute
      transfer 1000.00 EUR bank:main -> card:float
      [allow] TOP_UP_FLOOR: balance below the floor
        balance = 1500.00 EUR
        floor = 2000.00 EUR
        target = 5000.00 EUR
        need = 3500.00 EUR
        source = 1000.00 EUR
        amount = 1000.00 EUR
    card-float r1 (live): skip
      [block] INSUFFICIENT_BALANCE: the source cannot fund the top-up
        balance = 1500.00 EUR
        floor = 2000.00 EUR
        target = 5000.00 EUR
        need = 3500.00 EUR
        source = 10.00 EUR
    card-float r1 (live): skip
      [block] FLOOR_MET: balance at or above the floor
        balance = 2000.00 EUR
        floor = 2000.00 EUR
        target = 5000.00 EUR
    |}]
;;

let convert =
  rule
    "convert-usdc"
    (Convert
       { from = loc "exchange:spot"
       ; to_ = loc "bank:main"
       ; threshold = a "5000.000000 USDC"
       ; buy = eur
       ; minimum = a "100.000000 USDC"
       ; max_per_run = Some (a "2000.000000 USDC")
       })
;;

let%expect_test "convert sells the excess, capped per run, at the fresh rate" =
  let s = snapshot [ bal "exchange:spot" "8000.000000 USDC" ] in
  show convert { s with prices = eur_prices };
  show convert s;
  show
    convert
    { s with prices = [ price ~at:(at "2026-10-14T09:00:00Z") "EUR" "USDC" "1.08" ] };
  [%expect
    {|
    convert-usdc r1 (live): execute
      convert 2000.000000 USDC exchange:spot -> ~1851.85 EUR bank:main at 25/27
      [allow] CONVERT_EXCESS: excess above the threshold is converted
        balance = 8000.000000 USDC
        threshold = 5000.000000 USDC
        sell = 2000.000000 USDC
        rate = 25/27
        buy = 1851.85 EUR
    convert-usdc r1 (live): skip
      [block] PRICE_UNAVAILABLE: no USDC/EUR price within the allowed age
        pair = USDC/EUR
        max_age = 15m
    convert-usdc r1 (live): skip
      [block] PRICE_UNAVAILABLE: no USDC/EUR price within the allowed age
        pair = USDC/EUR
        max_age = 15m
    |}]
;;

let rebalance =
  rule
    "allocation"
    (Rebalance
       { targets =
           [ { location = loc "bank:main"; asset = eur; weight_bps = 5_000 }
           ; { location = loc "exchange:spot"; asset = usdc; weight_bps = 3_000 }
           ; { location = loc "exchange:spot"; asset = hype; weight_bps = 2_000 }
           ]
       ; band_bps = 500
       ; minimum = a "100.000000 USDC"
       })
;;

let%expect_test "rebalance moves value from overweight to underweight legs" =
  let prices = [ price "EUR" "USDC" "1.25"; price "HYPE" "USDC" "20" ] in
  (* 40000 EUR = 50000 USDC (50 %), 40000 USDC (40 %), 500 HYPE = 10000 USDC (10 %). *)
  show
    rebalance
    (snapshot
       ~prices
       [ bal "bank:main" "40000.00 EUR"
       ; bal "exchange:spot" "40000.000000 USDC"
       ; bal "exchange:spot" "500.00000000 HYPE"
       ]);
  (* Within 5 %: 52 %, 29 %, 19 %. *)
  show
    rebalance
    (snapshot
       ~prices
       [ bal "bank:main" "41600.00 EUR"
       ; bal "exchange:spot" "29000.000000 USDC"
       ; bal "exchange:spot" "950.00000000 HYPE"
       ]);
  [%expect
    {|
    allocation r1 (live): execute
      convert 10000.000000 USDC exchange:spot -> ~500.00000000 HYPE exchange:spot at 0.05
      [allow] REBALANCE_DRIFT: a leg drifted outside the band
        total = 100000.000000 USDC
        band_bps = 500
        bank:main EUR = 50000.000000 USDC, 0.00 bps from 5000
        exchange:spot USDC = 40000.000000 USDC, 1000.00 bps from 3000
        exchange:spot HYPE = 10000.000000 USDC, -1000.00 bps from 2000
    allocation r1 (live): skip
      [block] WITHIN_BAND: every leg is within the band
        total = 100000.000000 USDC
        band_bps = 500
        bank:main EUR = 52000.000000 USDC, 200.00 bps from 5000
        exchange:spot USDC = 29000.000000 USDC, -100.00 bps from 3000
        exchange:spot HYPE = 19000.000000 USDC, -100.00 bps from 2000
    |}]
;;

let yield_rule =
  rule
    "yield"
    (Yield
       { cash = loc "exchange:spot"
       ; fund = loc "fund:tbill"
       ; buffer = a "20000.000000 USDC"
       ; max_share_bps = 6_000
       ; minimum = a "500.000000 USDC"
       })
;;

let%expect_test "yield subscribes above the buffer, up to the share limit" =
  let s cash fund = snapshot [ bal "exchange:spot" cash; bal "fund:tbill" fund ] in
  show yield_rule (s "50000.000000 USDC" "10000.000000 USDC");
  show yield_rule (s "30000.000000 USDC" "40000.000000 USDC");
  show yield_rule (s "15000.000000 USDC" "40000.000000 USDC");
  show yield_rule (s "20100.000000 USDC" "0.000000 USDC");
  show yield_rule (s "40000.000000 USDC" "60000.000000 USDC");
  [%expect
    {|
    yield r1 (live): execute
      subscribe 26000.000000 USDC exchange:spot -> fund:tbill
      [allow] YIELD_SUBSCRIBE: cash above the buffer
        cash = 50000.000000 USDC
        fund = 10000.000000 USDC
        buffer = 20000.000000 USDC
        limit = 36000.000000 USDC
        amount = 26000.000000 USDC
    yield r1 (live): execute
      subscribe 2000.000000 USDC exchange:spot -> fund:tbill
      [allow] YIELD_SUBSCRIBE: cash above the buffer
        cash = 30000.000000 USDC
        fund = 40000.000000 USDC
        buffer = 20000.000000 USDC
        limit = 42000.000000 USDC
        amount = 2000.000000 USDC
    yield r1 (live): execute
      redeem 5000.000000 USDC fund:tbill -> exchange:spot
      [allow] YIELD_REDEEM_LIQUIDITY: cash below the buffer
        cash = 15000.000000 USDC
        fund = 40000.000000 USDC
        buffer = 20000.000000 USDC
        limit = 33000.000000 USDC
        amount = 5000.000000 USDC
    yield r1 (live): skip
      [block] BELOW_MINIMUM: cash above the buffer, but below the minimum
        cash = 20100.000000 USDC
        fund = 0.000000 USDC
        buffer = 20000.000000 USDC
        limit = 12060.000000 USDC
        amount = 100.000000 USDC
        minimum = 500.000000 USDC
    yield r1 (live): skip
      [block] ALLOCATION_LIMIT_REACHED: yield at its allocation limit
        cash = 40000.000000 USDC
        fund = 60000.000000 USDC
        buffer = 20000.000000 USDC
        limit = 60000.000000 USDC
    |}]
;;

let hedge =
  rule
    "stake-hedge"
    (Hedge
       { asset = hype
       ; staked = loc "staking:validator"
       ; position = loc "perp:HYPE"
       ; ratio_bps = 5_000
       ; band_bps = 1_000
       ; max_position = Some (a "400.00000000 HYPE")
       ; min_notional = a "50.000000 USDC"
       })
;;

let%expect_test "hedge keeps the short at the ratio within the band" =
  let prices = [ price "HYPE" "USDC" "25" ] in
  let s staked pos =
    snapshot ~prices [ bal "staking:validator" staked; bal "perp:HYPE" pos ]
  in
  show hedge (s "600.00000000 HYPE" "0.00000000 HYPE");
  show hedge (s "600.00000000 HYPE" "-290.00000000 HYPE");
  show hedge (s "600.00000000 HYPE" "-400.00000000 HYPE");
  show hedge (s "1000.00000000 HYPE" "-390.00000000 HYPE");
  show hedge (s "1000.00000000 HYPE" "-300.00000000 HYPE");
  [%expect
    {|
    stake-hedge r1 (live): execute
      hedge sell 300.00000000 HYPE perp:HYPE at 25
      [allow] HEDGE_ADJUST: the hedge drifted outside its band
        staked = 600.00000000 HYPE
        position = 0.00000000 HYPE
        target = -300.00000000 HYPE
        drift = -300.00000000 HYPE
        band = 30.00000000 HYPE
        price = 25
        notional = 7500.000000 USDC
    stake-hedge r1 (live): skip
      [block] WITHIN_BAND: the hedge is within its band
        staked = 600.00000000 HYPE
        position = -290.00000000 HYPE
        target = -300.00000000 HYPE
        drift = -10.00000000 HYPE
        band = 30.00000000 HYPE
    stake-hedge r1 (live): execute
      hedge buy 100.00000000 HYPE perp:HYPE at 25
      [allow] HEDGE_ADJUST: the hedge drifted outside its band
        staked = 600.00000000 HYPE
        position = -400.00000000 HYPE
        target = -300.00000000 HYPE
        drift = 100.00000000 HYPE
        band = 30.00000000 HYPE
        price = 25
        notional = 2500.000000 USDC
    stake-hedge r1 (live): skip
      [block] POSITION_LIMIT_REACHED: the hedge is at its maximum position
        staked = 1000.00000000 HYPE
        position = -390.00000000 HYPE
        target = -400.00000000 HYPE
        drift = -10.00000000 HYPE
        band = 40.00000000 HYPE
    stake-hedge r1 (live): execute
      hedge sell 100.00000000 HYPE perp:HYPE at 25
      [allow] HEDGE_ADJUST: the hedge drifted outside its band
        staked = 1000.00000000 HYPE
        position = -300.00000000 HYPE
        target = -400.00000000 HYPE
        drift = -100.00000000 HYPE
        band = 40.00000000 HYPE
        price = 25
        notional = 2500.000000 USDC
    |}]
;;
