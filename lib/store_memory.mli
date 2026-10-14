(** An in-memory {!Store.S} for tests, previews and offline modes. Not shared between
    processes; every operation is atomic because nothing yields inside it. *)

open! Core
include Store.S with type 'a io = 'a

(** An empty store. *)
val create : unit -> t
