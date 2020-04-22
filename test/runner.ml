open OUnit2

open SyntaxTest
open BuildTest
open CliTest

let test_syntax =
  "Syntax" >::: [
    LocTest.suite;
    AstTest.suite;
    FmtTest.suite;
    LexerTest.suite;
    ParserTest.suite
  ]

let test_build =
  "Build" >::: [
    OsTest.suite;
    SemverTest.suite;
    ImportPathTest.suite;
    CtxTest.suite;
    PackageTest.suite;
    ProjectTest.suite
  ]

let test_cli =
  "Command-Line Interface" >::: [
  ]

let suite =
  "CFN++" >::: [
    test_syntax;
    test_build;
    test_cli
  ]

let _ =
  run_test_tt_main suite
