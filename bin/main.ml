open! Core
open Treasury_rules

let or_exit = function
  | Ok x -> x
  | Error err ->
    eprint_s [%sexp (err : Error.t)];
    Stdlib.exit 2
;;

(* Tokens outside money's built-in registry (an exchange's own coin, say) must be
   registered before any file mentioning them is read. *)
let currencies_flag =
  Command.Param.(
    flag
      "-currency"
      (listed string)
      ~doc:"CODE:DIGITS register a crypto currency, e.g. HYPE:8 (repeatable)")
;;

let register_currencies specs =
  List.iter specs ~f:(fun spec ->
    match String.lsplit2 spec ~on:':' with
    | Some (code, digits) ->
      Money.Currency.register ~code ~exponent:(Int.of_string digits) ~kind:Crypto ()
      |> Or_error.ignore_m
      |> or_exit
    | None -> or_exit (Or_error.error_s [%message "-currency expects CODE:DIGITS" spec]))
;;

let load path of_sexp =
  or_exit (Or_error.try_with (fun () -> Sexp.load_sexp_conv_exn path of_sexp))
;;

let rules_flag =
  Command.Param.(flag "-rules" (required string) ~doc:"FILE a sexp list of rules")
;;

let check =
  Command.basic
    ~summary:"Validate a rules file and print one line per rule"
    (let%map_open.Command currencies = currencies_flag
     and path = rules_flag in
     fun () ->
       register_currencies currencies;
       let rules = load path [%of_sexp: Rule.t list] in
       List.iter rules ~f:(fun (r : Rule.t) ->
         printf
           "%s: %s, %s, every %s, %s\n"
           (Rule_id.to_string r.id)
           (Rule.Kind.name r.kind)
           (Mode.to_string r.mode)
           (Time_ns.Span.to_string (Cadence.span r.cadence))
           (Sexp.to_string [%sexp (r.status : Rule.Status.t)])))
;;

let evaluate =
  Command.basic
    ~summary:"Evaluate rules against a snapshot and print the proposals (records nothing)"
    ~readme:(fun () ->
      "Every rule is evaluated in its own mode, whatever its status, the way the \
       scheduler would at -now. Exit code 0.")
    (let%map_open.Command currencies = currencies_flag
     and rules_path = rules_flag
     and snapshot_path =
       flag "-snapshot" (required string) ~doc:"FILE balances, prices and recent runs"
     and settings_path = flag "-settings" (required string) ~doc:"FILE settings sexp"
     and now =
       flag
         "-now"
         (optional string)
         ~doc:"TIME evaluation time, ISO 8601 with offset (default: the snapshot's time)"
     and evidence =
       flag "-evidence" no_arg ~doc:" also print the evidence of every hit"
     in
     fun () ->
       register_currencies currencies;
       let rules = load rules_path [%of_sexp: Rule.t list] in
       let snapshot = load snapshot_path Snapshot.t_of_sexp in
       let settings = load settings_path Settings.t_of_sexp in
       let now =
         Option.value_map now ~default:snapshot.at ~f:Time_ns.of_string_with_utc_offset
       in
       List.iter rules ~f:(fun rule ->
         let o = Evaluate.evaluate settings rule snapshot ~now in
         print_endline (Outcome.to_string_hum o);
         if evidence
         then
           List.iter o.hits ~f:(fun (h : Hit.t) ->
             List.iter h.evidence ~f:(fun (k, v) -> printf "    %s = %s\n" k v))))
;;

let reason_codes =
  Command.basic
    ~summary:"List the reason codes with their group and meaning"
    (Command.Param.return (fun () ->
       List.iter Reason_code.builtin ~f:(fun code ->
         printf
           "%-26s %-6s %s\n"
           (Reason_code.to_string code)
           (Sexp.to_string [%sexp (Reason_code.group code : Reason_code.Group.t)])
           (Reason_code.description code))))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:
         "Treasury policy rules: check rule files and evaluate them against snapshots"
       [ "check", check; "evaluate", evaluate; "reason-codes", reason_codes ])
;;
