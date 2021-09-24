(* External Specifications *)

open OUnit2

(* Test Suite *)

let suite =
  "External Specifications" >::: [
    ParseTest.suite;
    AwsSpecTest.suite;
    CfnSpecTest.suite;
  ];
