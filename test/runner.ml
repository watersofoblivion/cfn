open OUnit2

let test_spec =
  "Specifications" >::: [
    SpecTest.AwsTest.suite;
    SpecTest.CfnTest.suite;
  ]

let test_core =
  "Core" >::: [
    CoreTest.TypeTest.suite;
    CoreTest.FmtTest.suite
  ]

let test_syntax =
  "Syntax" >::: [
    SyntaxTest.LocTest.suite;
    SyntaxTest.AstTest.suite;
    SyntaxTest.FmtTest.suite;
    SyntaxTest.LexerTest.suite;
    SyntaxTest.ParserTest.suite
  ]

let test_ir =
  "Intermediate Representation" >::: [
    IrTest.AnfTest.suite;
    IrTest.PkgTest.suite;
    IrTest.FmtTest.suite
  ]

let test_codegen =
  "Code Generation" >::: [
    CodegenTest.GcTest.suite;
    CodegenTest.HttpTest.suite;
    CodegenTest.JsonTest.suite;
    CodegenTest.TmplTest.suite
  ]

let test_build =
  "Build" >::: [
    BuildTest.OsTest.suite;
    BuildTest.SemverTest.suite;
    BuildTest.ImportPathTest.suite;
    BuildTest.CtxTest.suite;
    BuildTest.PackageTest.suite;
    BuildTest.ProjectTest.suite
  ]

let test_cli =
  "Command-Line Interface" >::: [
  ]

let suite =
  "CFN++" >::: [
    test_core;
    test_syntax;
    test_ir;
    test_codegen;
    test_build;
    test_cli
  ]

let _ =
  run_test_tt_main suite
