open OUnit2

include TypeTest
include AstTest
include PrettyTest

let suite =
  "Abstract Syntax" >::: [
    TypeTest.suite;
    AstTest.suite;
    PrettyTest.suite;
  ]
