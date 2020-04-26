open OUnit2

let suite =
  "AWS API" >::: [
    AwsSpecTest.ApiTest.suite;
    AwsSpecTest.DocTest.suite;
    AwsSpecTest.ExampleTest.suite;
    AwsSpecTest.PaginatorTest.suite;
    AwsSpecTest.SmokeTest.suite;
    AwsSpecTest.VersionTest.suite;
    AwsSpecTest.WaiterTest.suite
  ]
