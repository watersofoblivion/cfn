(* Desugaring *)

open OUnit2

include TypeTest
include OpTest
include PattTest
include AstTest
include FileTest

let suite =
  "Desugaring" >::: [
    TypeTest.suite;
    OpTest.suite;
    PattTest.suite;
    AstTest.suite;
    FileTest.suite;
  ]
