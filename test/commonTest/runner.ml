open OUnit2

let suite =
  "Common" >::: [
    CommonTest.LocTest.suite;
    CommonTest.SymTest.suite;
    CommonTest.PrettyTest.suite;
    CommonTest.EnvTest.suite;
  ]

let _ =
  run_test_tt_main suite
