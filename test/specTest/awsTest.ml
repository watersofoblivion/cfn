open OUnit2

let suite =
  "AWS API" >::: [
    AwsTest.ApiTest.suite;
    AwsTest.DocTest.suite;
    AwsTest.ExampleTest.suite;
    AwsTest.PaginatorTest.suite;
    AwsTest.SmokeTest.suite;
    AwsTest.VersionTest.suite;
    AwsTest.WaiterTest.suite
  ]
