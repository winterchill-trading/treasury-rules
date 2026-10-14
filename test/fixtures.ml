open! Core
open Money
open Treasury_rules

let hype = Currency.register ~code:"HYPE" ~exponent:8 ~kind:Crypto () |> Or_error.ok_exn
let eur = Currency.eur
let usdc = Currency.usdc

let a s =
  ignore (hype : Currency.t);
  Amount.of_string s
;;

let loc = Location.of_string
let id = Rule_id.of_string
let at s = Time_ns.of_string_with_utc_offset s
let now = at "2026-10-14T10:00:00Z"
let settings = Settings.default ~valuation:usdc

let price ?(at = now) base quote p =
  ignore (hype : Currency.t);
  Rate.create
    ~base:(Currency.of_string base)
    ~quote:(Currency.of_string quote)
    ~price:(Rate.price_of_string p |> Or_error.ok_exn)
    ~at
    ~source:"test"
  |> Or_error.ok_exn
;;

let bal l amount : Snapshot.Balance.t = { location = loc l; amount = a amount }

let snapshot ?(at = now) ?(prices = []) ?(history = []) ?(runs = []) balances =
  Snapshot.create ~at ~balances ~prices ~price_history:history ~runs ()
;;

let rule ?guards ?(mode = Mode.Live) ?cadence name kind =
  Rule.create ~id:(id name) ~name ~kind ?guards ?cadence ~mode () |> Or_error.ok_exn
;;

let show ?(settings = settings) ?(now = now) rule snapshot =
  let o = Evaluate.evaluate settings rule snapshot ~now in
  print_endline (Outcome.to_string_hum o);
  List.iter o.hits ~f:(fun (h : Hit.t) ->
    List.iter h.evidence ~f:(fun (k, v) -> printf "    %s = %s\n" k v))
;;
