open OUnit2

let suite =
  "CFN++" >::: [
    ExternalTest.suite;
  ]

let _ =
  run_test_tt_main suite
