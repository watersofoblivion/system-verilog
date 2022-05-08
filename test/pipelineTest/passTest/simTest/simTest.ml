open OUnit2

let suite =
  "Simulation" >::: [
    TimescaleTest.suite;
    EvtTest.suite;
    CycTest.suite;
  ]
