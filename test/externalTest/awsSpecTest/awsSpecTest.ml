(* AWS API Specification *)

open OUnit2

(* Test Suite *)

let suite =
  "AWS API Specification" >::: [
    ApiTest.suite;
    DocTest.suite;
    ExampleTest.suite;
    PaginatorTest.suite;
    SmokeTest.suite;
    VersionTest.suite;
    WaiterTest.suite
  ];
