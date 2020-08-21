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

let test_syntax =
  "Abstract Syntax" >::: [
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
  let runtime =
    "Runtime" >::: [
      RtTest.SyscallTest.suite;
      RtTest.LibcTest.suite;
      RtTest.UnwindTest.suite;
      RtTest.ExnTest.suite;
      RtTest.GcTest.suite;
      RtTest.HttpTest.suite;
      RtTest.JsonTest.suite;
      RtTest.XmlTest.suite;
      RtTest.RuntimeTest.suite
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
    test_syntax;
    test_ir;
    test_opt;
    test_codegen;
    test_cli
  ]

let _ =
  run_test_tt_main suite
