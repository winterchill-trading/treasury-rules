open! Core
open Treasury_rules
module Check = Store_conformance.Make (Monad.Ident) (Store_memory)

let%expect_test "the in-memory store passes the conformance suite" =
  print_s [%sexp (Check.check (Store_memory.create ()) : unit Or_error.t)];
  [%expect {| (Ok ()) |}]
;;
