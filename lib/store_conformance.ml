open! Core

(* The functor's [Store] argument hides the library module of that name. *)
module Claim = Store.Claim

module Make (M : Monad.S) (Store : Store.S with type 'a io := 'a M.t) = struct
  open M.Let_syntax

  let fail msg sexp = return (Or_error.error_s [%message msg (sexp : Sexp.t)])

  let ( let*? ) x f =
    M.bind x ~f:(function
      | Ok v -> f v
      | Error _ as e -> return e)
  ;;

  let t0 = Time_ns.of_string_with_utc_offset "2030-01-10T12:00:00Z"
  let minutes n = Time_ns.add t0 (Time_ns.Span.of_int_sec (60 * n))
  let stale_after = Time_ns.Span.of_int_sec 120

  let sweep id =
    Rule.create
      ~id:(Rule_id.of_string id)
      ~name:id
      ~kind:
        (Sweep
           { from = Location.of_string "a"
           ; to_ = Location.of_string "b"
           ; threshold = Money.Amount.of_string "0.00 EUR"
           ; minimum = Money.Amount.of_string "0.00 EUR"
           })
      ~mode:Live
      ()
    |> Or_error.ok_exn
  ;;

  let rules store =
    let rule = sweep "conformance-rule" in
    let*? () =
      match%bind Store.save_rule store ~expected_revision:None rule with
      | `Saved -> return (Ok ())
      | `Conflict _ -> fail "first insert conflicted" [%sexp ()]
    in
    let*? () =
      match%bind Store.save_rule store ~expected_revision:None rule with
      | `Conflict (Some _) -> return (Ok ())
      | _ -> fail "second insert of the same id did not conflict" [%sexp ()]
    in
    let paused = Rule.pause rule |> Or_error.ok_exn in
    let*? () =
      match%bind Store.save_rule store ~expected_revision:(Some 1) paused with
      | `Saved -> return (Ok ())
      | `Conflict _ -> fail "update at the right revision conflicted" [%sexp ()]
    in
    let*? () =
      match%bind Store.save_rule store ~expected_revision:(Some 1) paused with
      | `Conflict (Some (r : Rule.t)) when r.revision = 2 -> return (Ok ())
      | _ -> fail "update at a stale revision did not conflict" [%sexp ()]
    in
    match%bind Store.find_rule store rule.id with
    | Some r when Rule.equal r paused ->
      let%map all = Store.rules store in
      if List.exists all ~f:(Rule.equal paused)
      then Ok ()
      else Or_error.error_s [%message "rules does not list the saved rule"]
    | other -> fail "find_rule after update" [%sexp (other : Rule.t option)]
  ;;

  let key name at = Run_key.create (Rule_id.of_string name) Cadence.hourly ~now:at

  let claims store =
    let k = key "conformance-claim" t0 in
    let*? first =
      match%bind Store.claim_run store k ~mode:Live ~now:t0 ~stale_after with
      | Claimed r when r.attempt = 1 && r.version = 1 -> return (Ok r)
      | c -> fail "first claim" [%sexp (c : Claim.t)]
    in
    let*? () =
      match%bind Store.claim_run store k ~mode:Live ~now:(minutes 1) ~stale_after with
      | Running r when Run_record.equal r first -> return (Ok ())
      | c -> fail "fresh duplicate claim" [%sexp (c : Claim.t)]
    in
    let*? taken =
      match%bind Store.claim_run store k ~mode:Live ~now:(minutes 5) ~stale_after with
      | Claimed r when r.attempt = 2 && r.version = 2 -> return (Ok r)
      | c -> fail "take-over of a stale claim" [%sexp (c : Claim.t)]
    in
    (* The first runner's late write must lose. *)
    let failed =
      Run_record.fail first ~reason:"late" ~now:(minutes 6) |> Or_error.ok_exn
    in
    let*? () =
      match%bind Store.update_run store ~expected_version:first.version failed with
      | `Conflict (Some r) when Run_record.equal r taken -> return (Ok ())
      | _ -> fail "a stale version was not refused" [%sexp ()]
    in
    let settled =
      Run_record.fail taken ~reason:"done" ~now:(minutes 6) |> Or_error.ok_exn
    in
    let*? () =
      match%bind Store.update_run store ~expected_version:taken.version settled with
      | `Updated -> return (Ok ())
      | _ -> fail "update at the current version conflicted" [%sexp ()]
    in
    let*? () =
      match%bind Store.claim_run store k ~mode:Live ~now:(minutes 60) ~stale_after with
      | Settled r when Run_record.equal r settled -> return (Ok ())
      | c -> fail "claim of a settled run" [%sexp (c : Claim.t)]
    in
    match%bind Store.find_run store k with
    | Some r when Run_record.equal r settled -> return (Ok ())
    | r -> fail "find_run" [%sexp (r : Run_record.t option)]
  ;;

  let listing store =
    let claim name at =
      match%bind Store.claim_run store (key name at) ~mode:Live ~now:at ~stale_after with
      | Claimed r -> return r
      | c -> raise_s [%message "conformance: claim" (c : Claim.t)]
    in
    let old_at = Time_ns.sub t0 (Time_ns.Span.of_day 3.) in
    let%bind old = claim "conformance-list" old_at in
    let%bind recent = claim "conformance-list" t0 in
    let%bind other = claim "conformance-other" t0 in
    let%bind runs = Store.recent_runs store ~since:(minutes (-30)) in
    let keys = List.map runs ~f:(fun r -> r.key) in
    let*? () =
      if List.mem keys recent.key ~equal:Run_key.equal
         && List.mem keys other.key ~equal:Run_key.equal
         && not (List.mem keys old.key ~equal:Run_key.equal)
      then return (Ok ())
      else fail "recent_runs" [%sexp (keys : Run_key.t list)]
    in
    let%bind newest = Store.runs_of_rule store recent.key.rule ~limit:1 in
    match newest with
    | [ r ] when Run_key.equal r.key recent.key -> return (Ok ())
    | l ->
      fail
        "runs_of_rule newest first"
        [%sexp (List.map l ~f:(fun r -> r.Run_record.key) : Run_key.t list)]
  ;;

  let audit store =
    let entry n rule : Audit.t =
      { at = minutes n
      ; actor = System
      ; rule = Rule_id.of_string rule
      ; event = Run_approved (key rule t0)
      }
    in
    let entries =
      [ entry 1 "conformance-a"; entry 2 "conformance-b"; entry 3 "conformance-a" ]
    in
    let rec append = function
      | [] -> return ()
      | e :: rest ->
        let%bind () = Store.append_audit store e in
        append rest
    in
    let%bind () = append entries in
    let%bind a = Store.audit store ~rule:(Some (Rule_id.of_string "conformance-a")) in
    let%map all = Store.audit store ~rule:None in
    let mine = List.filter all ~f:(List.mem entries ~equal:Audit.equal) in
    if List.equal Audit.equal mine entries
       && List.equal Audit.equal a [ List.nth_exn entries 0; List.nth_exn entries 2 ]
    then Ok ()
    else Or_error.error_s [%message "audit order or filter"]
  ;;

  let check store =
    let*? () = rules store in
    let*? () = claims store in
    let*? () = listing store in
    audit store
  ;;
end
