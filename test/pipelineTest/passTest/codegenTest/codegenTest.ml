open OUnit2

include TypeTest
include BuiltinTest
include PattTest
include AtomTest
include ExprTest
include TermTest
include TopTest
include TmplTest

let suite =
  "LLVM Code Generation" >::: [
    TypeTest.suite;
    BuiltinTest.suite;
    PattTest.suite;
    AtomTest.suite;
    ExprTest.suite;
    TermTest.suite;
    TopTest.suite;
    TmplTest.suite;
  ]
