open OUnit2

let suite =
  "Pipeline" >::: [
    LangTest.suite;
    PassTest.suite;
  ]
