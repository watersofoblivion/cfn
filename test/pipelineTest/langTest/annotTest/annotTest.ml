open OUnit2

include TypeTest
include AstTest
include PrettyTest
include CheckTest

let suite =
  "Annotated Syntax" >::: [
    TypeTest.suite;
    AstTest.suite;
    PrettyTest.suite;
    CheckTest.suite;
  ]
