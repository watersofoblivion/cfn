open OUnit2

let suite =
  "CFN++" >::: [
    LocTest.suite;
    AstTest.suite;
    FmtTest.suite;
    LexerTest.suite;
    ParserTest.suite;
    BuildTest.suite;
    CliTest.suite
  ]


let _ = run_test_tt_main suite
