open OUnit2

let test_spec =
  "External" >::: [
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

let test_artifact =
  "Artifact" >::: [
    ArtifactTest.TestUtil.suite;
    ArtifactTest.OsTest.suite;
    ArtifactTest.SemverTest.suite;
    ArtifactTest.PathTest.suite;
    ArtifactTest.WorkspaceTest.suite
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
    test_artifact;
    test_cli
  ]

let _ =
  run_test_tt_main suite
