open OUnit2

let suite =
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

let _ =
  run_test_tt_main suite
