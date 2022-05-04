open OUnit2

let suite =
  "Common" >::: [
    TimescaleTest.suite;
  ]
