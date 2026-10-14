;; Example policy for an organisation valued in USDC. Amounts carry their currency.
(((id sweep-spot) (name "Sweep spot USDC above 50k") (mode Live) (status Active)
  (revision 1) (cadence 1h)
  (kind
   (Sweep (from exchange:spot) (to_ custody:cold) (threshold "50000.000000 USDC")
    (minimum "1000.000000 USDC")))
  (guards
   ((daily_cap "250000.000000 USDC") (approval_above "100000.000000 USDC"))))
 ((id payroll-runway) (name "Keep EUR payroll runway") (mode Live)
  (status Active) (revision 1) (cadence 1d)
  (kind
   (Convert (from exchange:spot) (to_ bank:eur) (threshold "20000.000000 USDC")
    (buy EUR) (minimum "500.000000 USDC") (max_per_run "30000.000000 USDC")))
  (guards
   ((drawdown ((asset HYPE) (max_drawdown_bps 1500) (lookback 7d))))))
 ((id idle-yield) (name "Place idle cash in the bill fund") (mode Shadow)
  (status Active) (revision 1) (cadence 1d)
  (kind
   (Yield (cash custody:cold) (fund fund:tbill) (buffer "100000.000000 USDC")
    (max_share_bps 5000) (minimum "5000.000000 USDC")))
  (guards ()))
 ((id stake-hedge) (name "Hedge half of staked HYPE") (mode Live)
  (status Active) (revision 1) (cadence 1h)
  (kind
   (Hedge (asset HYPE) (staked staking:validator) (position perp:HYPE)
    (ratio_bps 5000) (band_bps 1000) (max_position "5000.00000000 HYPE")
    (min_notional "100.000000 USDC")))
  (guards ())))
