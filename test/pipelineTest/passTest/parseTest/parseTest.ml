open OUnit2

include TypeTest
include OpTest
include PattTest
include ExprTest
include TopTest
include ImportTest
include FileTest

let suite =
  "Parsing" >::: [
    TypeTest.suite;
    OpTest.suite;
    PattTest.suite;
    ExprTest.suite;
    TopTest.suite;
    ImportTest.suite;
    FileTest.suite;
  ]
