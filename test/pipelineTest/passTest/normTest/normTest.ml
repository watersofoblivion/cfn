open OUnit2

include TypeTest
include BuiltinTest
include PattTest
include ExprTest
include TopTest

let suite =
  "A-Normalization" >::: [
    TypeTest.suite;
    BuiltinTest.suite;
    PattTest.suite;
    ExprTest.suite;
    TopTest.suite;
  ]
