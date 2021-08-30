open OUnit2

include ParserTest

let suite =
  "Parsing" >::: [
    ParserTest.suite;
  ]
