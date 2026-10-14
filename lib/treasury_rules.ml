(** Treasury policy rules evaluated over snapshots, with exactly-once runs and audit.

    {!Rule} describes a policy, {!Evaluate} turns a {!Rule} and a {!Snapshot} into an
    {!Outcome} (proposed {!Action}s with {!Hit}s and {!Reason_code}s), {!Run_record}
    tracks one run per rule and period, {!Runner} drives rules against a {!Store}, and
    {!Audit} records every change. *)

module Action = Action
module Audit = Audit
module Cadence = Cadence
module Evaluate = Evaluate
module Hit = Hit

(** Validated identifiers; {!Rule_id} and {!Location} are built with it. *)
module Id = Id

module Location = Location
module Mode = Mode
module Outcome = Outcome
module Reason_code = Reason_code
module Rule = Rule
module Rule_id = Rule_id
module Run_key = Run_key
module Run_record = Run_record
module Runner = Runner
module Settings = Settings
module Snapshot = Snapshot
module Store = Store
module Store_conformance = Store_conformance
module Store_memory = Store_memory
