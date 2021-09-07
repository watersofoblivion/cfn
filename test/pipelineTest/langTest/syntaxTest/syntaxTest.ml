open OUnit2

include TypeTest
include OpTest
include PattTest
include AstTest
include ImportTest
include FileTest

let suite =
  "Abstract Syntax" >::: [
    TypeTest.suite;
    OpTest.suite;
    PattTest.suite;
    AstTest.suite;
    ImportTest.suite;
    FileTest.suite;
  ]
