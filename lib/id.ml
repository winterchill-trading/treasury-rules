open! Core

module type S = sig
  type t [@@deriving bin_io, sexp, compare, equal, hash]

  include Identifiable.S with type t := t

  val create : string -> t Or_error.t
  val create_exn : string -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

module Make (Spec : sig
    val module_name : string
    val what : string
    val forbidden : char list
  end) =
struct
  let invalid s =
    String.is_empty s
    || String.length s > 128
    || String.exists s ~f:(fun c ->
      Char.is_whitespace c || List.mem Spec.forbidden c ~equal:Char.equal)
  ;;

  let of_repr s =
    if invalid s then raise_s [%message ("invalid " ^ Spec.what) (s : string)] else s
  ;;

  module Stable = struct
    module V1 = struct
      module Repr = struct
        type t = string [@@deriving bin_io, sexp, compare, equal, hash]
      end

      type t = string [@@deriving compare, equal, hash, sexp_of]

      let t_of_sexp sexp = of_repr (Repr.t_of_sexp sexp)

      (* Validate on read, so a damaged record cannot smuggle a separator into a key. *)
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

  module T = struct
    include Stable.V1

    let module_name = Spec.module_name
    let to_string = Fn.id
    let of_string = of_repr
  end

  include T
  include Identifiable.Make (T)

  let create s = Or_error.try_with (fun () -> of_repr s)
  let create_exn = of_repr
end
