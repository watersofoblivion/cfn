(* Languages *)

open OUnit2

(* Test Suite *)

let suite =
  "Languages" >::: [
    SyntaxTest.suite;
    AnnotTest.suite;
    IrTest.suite;
    MonoTest.suite;
    ClosTest.suite;
  ];
