open OUnit2

let suite =
  "CFN++ Compiler" >::: [
    AstTest.suite
  ]

let _ =
  run_test_tt_main suite
