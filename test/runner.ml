open OUnit2

let suite =
  "System Verilog" >::: [
    CommonTest.suite;
    PipelineTest.suite;
    ToolchainTest.suite;
  ]

let _ =
  run_test_tt_main suite
