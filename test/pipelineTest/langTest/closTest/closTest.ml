open OUnit2

include TypeTest
include BuiltinTest
include PattTest
include AstTest

let suite =
  "Closure Converted" >::: [
    TypeTest.suite;
    BuiltinTest.suite;
    PattTest.suite;
    AstTest.suite;
  ]
