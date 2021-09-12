(* Desugaring *)

open OUnit2

include TypeTest
include OpTest
include PattTest
include ExprTest
include TopTest
include FileTest

let suite =
  "Desugaring" >::: [
    TypeTest.suite;
    OpTest.suite;
    PattTest.suite;
    ExprTest.suite;
    TopTest.suite;
    FileTest.suite;
  ]
