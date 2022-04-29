open OUnit2

let suite =
  "Passes" >::: [
    ParseTest.suite;
    NormTest.suite;
    ElabTest.suite;
    SimTest.suite;
  ]
