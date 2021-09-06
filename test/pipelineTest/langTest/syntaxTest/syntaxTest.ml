open OUnit2

include TypeTest
include OpTest
include AstTest
include PrettyTest

let suite =
  "Abstract Syntax" >::: [
    TypeTest.suite;
    OpTest.suite;
    AstTest.suite;
    PrettyTest.suite;
  ]
