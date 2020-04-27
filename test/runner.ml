open OUnit2

let test_spec =
  "Specifications" >::: [
    SpecTest.AwsTest.suite;
    SpecTest.CfnTest.suite;
  ]

let test_syntax =
  "Syntax" >::: [
    SyntaxTest.LocTest.suite;
    SyntaxTest.TypeTest.suite;
    SyntaxTest.AstTest.suite;
    SyntaxTest.FmtTest.suite;
    SyntaxTest.LexerTest.suite;
    SyntaxTest.ParserTest.suite
  ]

let test_ir =
  "Intermediate Representation" >::: [
    IrTest.TypeTest.suite;
    IrTest.AnfTest.suite;
    IrTest.PkgTest.suite;
    IrTest.FmtTest.suite
  ]

let test_opt =
  "Optimizer" >::: [
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
    test_spec;
    test_syntax;
    test_ir;
    test_opt;
    test_codegen;
    test_build;
    test_cli
  ]

let _ =
  run_test_tt_main suite
