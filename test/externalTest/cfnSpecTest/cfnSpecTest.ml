(* CloudFormation Specification *)

open OUnit2

(* Test Suite *)

let suite =
  "CloudFormation Specification" >::: [
    CfnTest.suite;
    PrimitiveTest.suite;
    PropertyTest.suite;
    ResourceTest.suite;
    UpdateTest.suite
  ];
