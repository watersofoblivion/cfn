(* Optimizer *)

open OUnit2

(* Test Suite *)

let suite =
  "Common" >::: [
    LocTest.suite;
    SymTest.suite;
    PrettyTest.suite;
    EnvTest.suite;
  ];
