open OUnit2

let suite =
  "Simulation" >::: [
    EvtTest.suite;
    CycTest.suite;
  ]
