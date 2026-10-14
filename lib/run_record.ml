open! Core

module Status = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Evaluating
        | Skipped
        | Shadowed
        | Awaiting_approval
        | Awaiting_execution
        | Executed
        | Failed
        | Rejected
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    | Evaluating
    | Skipped
    | Shadowed
    | Awaiting_approval
    | Awaiting_execution
    | Executed
    | Failed
    | Rejected
  [@@deriving sexp, compare, equal, enumerate]

  let is_final = function
    | Skipped | Shadowed | Executed | Failed | Rejected -> true
    | Evaluating | Awaiting_approval | Awaiting_execution -> false
  ;;
end

module Repr = struct
  type t =
    { key : Run_key.Stable.V1.t
    ; mode : Mode.Stable.V1.t
    ; status : Status.Stable.V1.t
    ; attempt : int
    ; outcome : Outcome.Stable.V1.t option [@sexp.option]
    ; note : string option [@sexp.option]
    ; version : int
    ; created_at : Time_ns.Stable.Alternate_sexp.V1.t
    ; updated_at : Time_ns.Stable.Alternate_sexp.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { key : Run_key.t
  ; mode : Mode.t
  ; status : Status.t
  ; attempt : int
  ; outcome : Outcome.t option [@sexp.option]
  ; note : string option [@sexp.option]
  ; version : int
  ; created_at : Time_ns.Alternate_sexp.t
  ; updated_at : Time_ns.Alternate_sexp.t
  }
[@@deriving sexp_of, compare, equal]

let claim key ~mode ~now =
  { key
  ; mode
  ; status = Evaluating
  ; attempt = 1
  ; outcome = None
  ; note = None
  ; version = 1
  ; created_at = now
  ; updated_at = now
  }
;;

let is_stale t ~now ~stale_after =
  match t.status with
  | Evaluating -> Time_ns.( < ) (Time_ns.add t.updated_at stale_after) now
  | _ -> false
;;

let refused t what =
  Or_error.error_s
    [%message
      "run status does not allow this change"
        what
        ~run:(Run_key.to_string t.key : string)
        ~status:(t.status : Status.t)]
;;

let step t status ~now = { t with status; version = t.version + 1; updated_at = now }

let take_over t ~now =
  match t.status with
  | Evaluating -> Ok { (step t Evaluating ~now) with attempt = t.attempt + 1 }
  | _ -> refused t "take over"
;;

let record t (outcome : Outcome.t) ~now =
  match t.status with
  | Evaluating when not (Rule_id.equal outcome.rule t.key.rule) ->
    Or_error.error_s
      [%message
        "outcome is for another rule"
          ~run:(Run_key.to_string t.key : string)
          ~outcome:(outcome.rule : Rule_id.t)]
  | Evaluating ->
    let status : Status.t =
      match t.mode, outcome.route with
      | _, Skip -> Skipped
      | (Shadow | Dry_run), (Execute | Approve) -> Shadowed
      | Live, Approve -> Awaiting_approval
      | Live, Execute -> Awaiting_execution
    in
    Ok { (step t status ~now) with outcome = Some outcome }
  | _ -> refused t "record"
;;

let approve t ~now =
  match t.status with
  | Awaiting_approval -> Ok (step t Awaiting_execution ~now)
  | _ -> refused t "approve"
;;

let reject t ~note ~now =
  match t.status with
  | Awaiting_approval -> Ok { (step t Rejected ~now) with note = Some note }
  | _ -> refused t "reject"
;;

let executed t ~now =
  match t.status with
  | Awaiting_execution -> Ok (step t Executed ~now)
  | _ -> refused t "mark executed"
;;

let fail t ~reason ~now =
  match t.status with
  | Evaluating | Awaiting_execution -> Ok { (step t Failed ~now) with note = Some reason }
  | _ -> refused t "fail"
;;

let actions t =
  match t.outcome with
  | None -> []
  | Some o -> o.actions
;;

let is_pending t =
  match t.status with
  | Awaiting_approval | Awaiting_execution -> true
  | _ -> false
;;

let counts_against_caps t =
  match t.mode, t.status with
  | Live, (Awaiting_approval | Awaiting_execution | Executed) -> true
  | _ -> false
;;

let check t =
  let bad what = Or_error.error_s [%message "inconsistent run record" what] in
  if t.attempt < 1 || t.version < 1
  then bad "attempt or version below 1"
  else (
    match t.status, t.outcome with
    | Evaluating, Some _ -> bad "evaluating run has an outcome"
    | ( (Skipped | Shadowed | Awaiting_approval | Awaiting_execution | Executed | Rejected)
      , None ) -> bad "settled run has no outcome"
    | _, Some o when not (Rule_id.equal o.rule t.key.rule) ->
      bad "outcome for another rule"
    | _ -> Ok t)
;;

let of_repr r = Or_error.ok_exn (check r)
let t_of_sexp sexp = of_repr (Repr.t_of_sexp sexp)

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, sexp_of]

    let t_of_sexp = t_of_sexp

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type nonrec t = t

          let to_binable = Fn.id
          let of_binable = of_repr
        end)
  end
end
