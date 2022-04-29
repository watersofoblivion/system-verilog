open OUnit2

let suite =
  "Toolchain" >::: [
    CliTest.suite;
  ]
