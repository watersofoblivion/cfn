open OUnit2

include TypeTest
include BuiltinTest
include PattTest
include AtomTest
include ExprTest
include TermTest
include TopTest

let suite =
  "ANF Intermediate Representation" >::: [
    TypeTest.suite;
    BuiltinTest.suite;
    PattTest.suite;
    AtomTest.suite;
    ExprTest.suite;
    TermTest.suite;
    TopTest.suite;
  ]
