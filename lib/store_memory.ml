open! Core

type 'a io = 'a

type t =
  { rules : Rule.t Rule_id.Table.t
  ; runs : Run_record.t Run_key.Table.t
  ; audit : Audit.t Queue.t
  }

let create () =
  { rules = Rule_id.Table.create ()
  ; runs = Run_key.Table.create ()
  ; audit = Queue.create ()
  }
;;

let find_rule t id = Hashtbl.find t.rules id

let rules t =
  Hashtbl.data t.rules
  |> List.sort ~compare:(fun (a : Rule.t) b -> Rule_id.compare a.id b.id)
;;

let save_rule t ~expected_revision (rule : Rule.t) =
  let current = Hashtbl.find t.rules rule.id in
  match expected_revision, current with
  | None, None ->
    Hashtbl.set t.rules ~key:rule.id ~data:rule;
    `Saved
  | Some r, Some c when c.revision = r ->
    Hashtbl.set t.rules ~key:rule.id ~data:rule;
    `Saved
  | _ -> `Conflict current
;;

let claim_run t key ~mode ~now ~stale_after : Store.Claim.t =
  match Hashtbl.find t.runs key with
  | None ->
    let r = Run_record.claim key ~mode ~now in
    Hashtbl.set t.runs ~key ~data:r;
    Claimed r
  | Some r when Run_record.is_stale r ~now ~stale_after ->
    let r = Run_record.take_over r ~now |> Or_error.ok_exn in
    Hashtbl.set t.runs ~key ~data:r;
    Claimed r
  | Some r ->
    (match r.status with
     | Evaluating -> Running r
     | _ -> Settled r)
;;

let update_run t ~expected_version (r : Run_record.t) =
  match Hashtbl.find t.runs r.key with
  | Some current when current.version = expected_version ->
    Hashtbl.set t.runs ~key:r.key ~data:r;
    `Updated
  | current -> `Conflict current
;;

let find_run t key = Hashtbl.find t.runs key

let recent_runs t ~since =
  Hashtbl.data t.runs
  |> List.filter ~f:(fun (r : Run_record.t) ->
    Time_ns.( >= ) r.key.period_start since || Run_record.is_pending r)
  |> List.sort ~compare:(fun (a : Run_record.t) b -> Run_key.compare a.key b.key)
;;

let runs_of_rule t rule ~limit =
  Hashtbl.data t.runs
  |> List.filter ~f:(fun (r : Run_record.t) -> Rule_id.equal r.key.rule rule)
  |> List.sort ~compare:(fun (a : Run_record.t) b ->
    Time_ns.compare b.key.period_start a.key.period_start)
  |> fun l -> List.take l limit
;;

let append_audit t entry = Queue.enqueue t.audit entry

let audit t ~rule =
  Queue.to_list t.audit
  |> List.filter ~f:(fun (e : Audit.t) ->
    Option.value_map rule ~default:true ~f:(Rule_id.equal e.rule))
;;
