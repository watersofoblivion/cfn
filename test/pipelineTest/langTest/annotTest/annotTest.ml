open OUnit2

include TypeTest
include BuiltinTest
include PattTest
include AstTest

let suite =
  "Annotated Syntax" >::: [
    TypeTest.suite;
    BuiltinTest.suite;
    PattTest.suite;
    AstTest.suite;
  ]
