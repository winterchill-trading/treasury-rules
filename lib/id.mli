(** Validated string identifiers chosen by the caller (a database key, a UUID, a short
    name). Reading one from sexp or bin_prot validates it like {!S.create}, so a damaged
    record cannot carry a malformed id into a run key. *)

open! Core

module type S = sig
  type t [@@deriving bin_io, sexp, compare, equal, hash]

  include Identifiable.S with type t := t

  (** Fails on an empty string, one longer than 128 bytes, or one containing whitespace
      or a character the id kind refuses. *)
  val create : string -> t Or_error.t

  (** Raises on what {!create} rejects. *)
  val create_exn : string -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

(** [Make (Spec)] builds an id kind. [forbidden] lists characters refused on top of
    whitespace; [what] names the kind in error messages. *)
module Make (_ : sig
    val module_name : string
    val what : string
    val forbidden : char list
  end) : S
