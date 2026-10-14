open! Core

module Result = struct
  type t =
    | Recorded of Run_record.t
    | Already of Run_record.t
    | Not_scheduled of Rule_id.t
  [@@deriving sexp_of]
end

module Make (M : Monad.S) (Store : Store.S with type 'a io := 'a M.t) = struct
  open M.Let_syntax

  let audit store ~actor ~now rule event =
    Store.append_audit store { Audit.at = now; actor; rule; event }
  ;;

  let create_rule store ~actor ~now (rule : Rule.t) =
    match%bind Store.save_rule store ~expected_revision:None rule with
    | `Conflict _ ->
      return (Or_error.error_s [%message "rule exists" ~id:(rule.id : Rule_id.t)])
    | `Saved ->
      let%map () = audit store ~actor ~now rule.id (Rule_created rule) in
      Ok ()
  ;;

  let change_rule store ~actor ~now id ~f =
    match%bind Store.find_rule store id with
    | None -> return (Or_error.error_s [%message "no such rule" (id : Rule_id.t)])
    | Some (before : Rule.t) ->
      (match f before with
       | Error _ as e -> return e
       | Ok (after : Rule.t) ->
         if not (Rule_id.equal after.id id)
         then return (Or_error.error_s [%message "a change cannot rename a rule" (id : Rule_id.t)])
         else (
           match%bind Store.save_rule store ~expected_revision:(Some before.revision) after with
           | `Conflict _ ->
             return
               (Or_error.error_s
                  [%message "rule changed concurrently; read it again" (id : Rule_id.t)])
           | `Saved ->
             let event : Audit.Event.t =
               if Rule.Status.equal before.status after.status
               then Rule_updated after
               else
                 Rule_status_changed
                   { from = before.status; to_ = after.status; revision = after.revision }
             in
             let%map () = audit store ~actor ~now id event in
             Ok after))
  ;;

  let start_of_day now =
    Time_ns.of_date_ofday
      ~zone:Time_float.Zone.utc
      (Time_ns.to_date now ~zone:Time_float.Zone.utc)
      Time_ns.Ofday.start_of_day
  ;;

  let with_runs store (snapshot : Snapshot.t) ~now =
    let%map runs = Store.recent_runs store ~since:(start_of_day now) in
    { snapshot with runs }
  ;;

  let codes (o : Outcome.t) = List.map o.hits ~f:(fun h -> h.code)

  let run_rule store settings snapshot ~now ~actor (rule : Rule.t) =
    if not (Rule.is_scheduled rule)
    then return (Result.Not_scheduled rule.id)
    else (
      let key = Run_key.create rule.id rule.cadence ~now in
      match%bind
        Store.claim_run
          store
          key
          ~mode:rule.mode
          ~now
          ~stale_after:settings.Settings.stale_claim_after
      with
      | Running r | Settled r -> return (Result.Already r)
      | Claimed claimed ->
        let%bind () =
          audit store ~actor ~now rule.id (Run_claimed { key; attempt = claimed.attempt })
        in
        let%bind snapshot = with_runs store snapshot ~now in
        let outcome = Evaluate.evaluate settings rule snapshot ~now in
        let recorded = Run_record.record claimed outcome ~now |> Or_error.ok_exn in
        (match%bind Store.update_run store ~expected_version:claimed.version recorded with
         | `Conflict (Some other) -> return (Result.Already other)
         | `Conflict None ->
           (* The claim vanished: a store bug or a manual delete. Report what we have
              rather than invent a record. *)
           return (Result.Already claimed)
         | `Updated ->
           let%map () =
             audit
               store
               ~actor
               ~now
               rule.id
               (Run_recorded { key; status = recorded.status; codes = codes outcome })
           in
           Result.Recorded recorded))
  ;;

  let tick store settings snapshot ~now ~actor =
    let%bind rules = Store.rules store in
    let rec loop acc = function
      | [] -> return (List.rev acc)
      | rule :: rest ->
        let%bind r = run_rule store settings snapshot ~now ~actor rule in
        loop (r :: acc) rest
    in
    loop [] rules
  ;;

  let preview store settings snapshot ~now rule =
    let%map snapshot = with_runs store snapshot ~now in
    Evaluate.evaluate ~mode:Dry_run settings rule snapshot ~now
  ;;

  let transition store ~actor ~now key ~f ~event =
    match%bind Store.find_run store key with
    | None ->
      return (Or_error.error_s [%message "no such run" ~run:(Run_key.to_string key : string)])
    | Some (r : Run_record.t) ->
      (match f r with
       | Error _ as e -> return e
       | Ok (r' : Run_record.t) ->
         (match%bind Store.update_run store ~expected_version:r.version r' with
          | `Conflict _ ->
            return
              (Or_error.error_s
                 [%message
                   "run changed concurrently; read it again"
                     ~run:(Run_key.to_string key : string)])
          | `Updated ->
            let%map () = audit store ~actor ~now key.rule event in
            Ok r'))
  ;;

  let approve store ~actor ~now key =
    transition store ~actor ~now key ~f:(Run_record.approve ~now) ~event:(Run_approved key)
  ;;

  let reject store ~actor ~now ~note key =
    transition
      store
      ~actor
      ~now
      key
      ~f:(Run_record.reject ~note ~now)
      ~event:(Run_rejected { key; note })
  ;;

  let executed store ~actor ~now key =
    transition store ~actor ~now key ~f:(Run_record.executed ~now) ~event:(Run_executed key)
  ;;

  let failed store ~actor ~now ~reason key =
    transition
      store
      ~actor
      ~now
      key
      ~f:(Run_record.fail ~reason ~now)
      ~event:(Run_failed { key; reason })
  ;;
end
