open OUnit2

let suite =
  "Passes" >::: [
    PreTest.suite;
    ParseTest.suite;
    NormTest.suite;
    DocTest.suite;
    ElabTest.suite;
    SimTest.suite;
    PnrTest.suite;
  ]
