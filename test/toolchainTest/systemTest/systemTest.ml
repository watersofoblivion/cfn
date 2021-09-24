(* Operating System Integration *)

open OUnit2

(* Test Suite *)

let suite =
  "Operating System Integration" >::: [
    TestUtil.suite;
    OsTest.suite;
    SemverTest.suite;
    GitTest.suite;
    DockerTest.suite
  ]
