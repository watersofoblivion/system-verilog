open OUnit2

let suite =
  "System Verilog" >::: [
    PipelineTest.suite;
    ToolchainTest.suite;
  ]

let _ =
  run_test_tt_main suite
