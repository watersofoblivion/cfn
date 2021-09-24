(* Pipeline *)

open OUnit2

(* Test Suite *)

let suite =
  "Toolchain" >::: [
    SourceTest.suite;
    SystemTest.suite;
    CliTest.suite;
  ];
