open OUnit2

let test_spec =
  "External Specifications" >::: [
    ExtTest.ParseTest.suite;
    "AWS API Specification" >::: [
      AwsSpecTest.ApiTest.suite;
      AwsSpecTest.DocTest.suite;
      AwsSpecTest.ExampleTest.suite;
      AwsSpecTest.PaginatorTest.suite;
      AwsSpecTest.SmokeTest.suite;
      AwsSpecTest.VersionTest.suite;
      AwsSpecTest.WaiterTest.suite
    ];
    "CloudFormation Specification" >::: [
      CfnSpecTest.CfnTest.suite;
      CfnSpecTest.PrimitiveTest.suite;
      CfnSpecTest.PropertyTest.suite;
      CfnSpecTest.ResourceTest.suite;
      CfnSpecTest.UpdateTest.suite
    ]
  ]

let test_system =
  "Operating System Integration" >::: [
    SystemTest.TestUtil.suite;
    SystemTest.OsTest.suite;
    SystemTest.SemverTest.suite;
    SystemTest.GitTest.suite;
    SystemTest.DockerTest.suite
  ]

let test_source =
  "Source Code Management" >::: [
    SourceTest.PathTest.suite;
    SourceTest.WorkspaceTest.suite
  ]

let test_common =
  "Common" >::: [
    CommonTest.LocTest.suite;
    CommonTest.SymTest.suite;
    CommonTest.PrettyTest.suite;
    CommonTest.EnvTest.suite;
    CommonTest.LexTest.suite;
  ]

let test_syntax =
  "Abstract Syntax" >::: [
    SyntaxTest.TypeTest.suite;
    SyntaxTest.AstTest.suite;
    SyntaxTest.FmtTest.suite;
    SyntaxTest.LexerTest.suite;
    SyntaxTest.ParserTest.suite
  ]

let test_annot =
  "Annotated" >::: [
    AnnotTest.TypeTest.suite;
    AnnotTest.AstTest.suite;
    AnnotTest.FmtTest.suite;
    AnnotTest.CheckTest.suite;
  ]

let test_ir =
  "ANF Intermediate Representation" >::: [
    IrTest.TypeTest.suite;
    IrTest.AstTest.suite;
    IrTest.PkgTest.suite;
    IrTest.FmtTest.suite;
    IrTest.CheckTest.suite;
  ]

let test_opt =
  "Optimizer" >::: [
  ]

let test_mono =
  "Monomorphic" >::: [
    MonoTest.TypeTest.suite;
    MonoTest.AstTest.suite;
    MonoTest.FmtTest.suite;
    MonoTest.CheckTest.suite;
  ]

let test_clos =
  "Closure Converted" >::: [
    ClosTest.TypeTest.suite;
    ClosTest.AstTest.suite;
    ClosTest.FmtTest.suite;
    ClosTest.CheckTest.suite;
  ]

let test_desug =
  "Desugaring" >::: [
  ]

let test_norm =
  "Normalization" >::: [
  ]

let test_monomorph =
  "Monomorphization" >::: [
  ]

let test_conv =
  "Closure Conversion" >::: [
  ]

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
    CodegenTest.TmplTest.suite;
  ]

let test_cli =
  "Command-Line Interface" >::: [
  ]

let suite =
  "CFN++" >::: [
    test_spec;
    test_system;
    test_source;
    test_common;
    test_syntax;
    test_annot;
    test_ir;
    test_opt;
    test_mono;
    test_clos;
    test_desug;
    test_norm;
    test_monomorph;
    test_conv;
    test_codegen;
    test_cli
  ]

let _ =
  run_test_tt_main suite
