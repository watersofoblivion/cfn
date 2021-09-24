(* Pipeline *)

open OUnit2

(* Test Suite *)

let suite =
  "Pipeline" >::: [
    LangTest.suite;
    PassTest.suite;
  ];
