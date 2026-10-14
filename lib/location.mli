(** A named place where an organisation holds an asset: a bank account (["bank:EUR"]), an
    exchange sub-account (["exchange:spot"]), a fund holding (["fund:tbill"]), a staking
    position. The library only compares names; the service maps them to its accounts. *)

include Id.S
