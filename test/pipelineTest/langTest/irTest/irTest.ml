open OUnit2

include TypeTest
include BuiltinTest
include AstTest
include PrettyTest
include CheckTest

let suite =
  "ANF Intermediate Representation" >::: [
    TypeTest.suite;
    BuiltinTest.suite;
    AstTest.suite;
    PrettyTest.suite;
    CheckTest.suite;
  ]
