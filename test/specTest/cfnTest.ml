open OUnit2

open

let suite =
  "CloudFormation" >::: [
    CfnSpecTest.CfnTest.suite;
    CfnSpecTest.PrimitiveTest.suite;
    CfnSpecTest.PropertyTest.suite;
    CfnSpecTest.ResourceTest.suite;
    CfnSpecTest.UpdateTest.suite
  ]
