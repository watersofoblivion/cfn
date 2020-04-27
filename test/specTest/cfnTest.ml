open OUnit2

open

let suite =
  "CloudFormation" >::: [
    CfnTest.CfnTest.suite;
    CfnTest.PrimitiveTest.suite;
    CfnTest.PropertyTest.suite;
    CfnTest.ResourceTest.suite;
    CfnTest.UpdateTest.suite
  ]
