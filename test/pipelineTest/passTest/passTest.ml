(* Passes *)

open OUnit2

(* Test Suite *)

let suite =
  "Passes" >::: [
    ParseTest.suite;
    DesugTest.suite;
    NormTest.suite;
    MonomorphTest.suite;
    ConvTest.suite;
    CodegenTest.suite;
  ];
