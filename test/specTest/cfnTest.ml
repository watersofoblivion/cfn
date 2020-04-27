open OUnit2

let suite =
  "CloudFormation" >::: [
    CfnTest.CfnTest.suite;
    CfnTest.PrimitiveTest.suite;
    CfnTest.PropertyTest.suite;
    CfnTest.ResourceTest.suite;
    CfnTest.UpdateTest.suite
  ]
