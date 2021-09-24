(* Source Code Management *)

open OUnit2

(* Test Suite *)

let suite =
  "Sources" >::: [
    PathTest.suite;
    WorkspaceTest.suite
  ]
