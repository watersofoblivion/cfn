open OUnit2

let test_codegen =
  let runtime =
    "Runtime" >::: [
      RuntimeTest.TypesTest.suite;
      RuntimeTest.CryptoTest.suite;
      RuntimeTest.LibcTest.suite;
      RuntimeTest.ExnTest.suite;
      RuntimeTest.GcTest.suite;
      RuntimeTest.HttpTest.suite;
      RuntimeTest.JsonTest.suite;
      RuntimeTest.XmlTest.suite;
      RuntimeTest.SystemTest.suite
    ]
  in
  "Code Generation" >::: [
    runtime;
  ]

let suite =
  "CFN++" >::: [
    ExternalTest.suite;
    "Common" >::: [
      CommonTest.LocTest.suite;
      CommonTest.SymTest.suite;
      CommonTest.PrettyTest.suite;
      CommonTest.EnvTest.suite;
    ];
    PipelineTest.suite;
    OptTest.suite;
    test_codegen;
    ToolchainTest.suite;
  ]

let _ =
  run_test_tt_main suite
