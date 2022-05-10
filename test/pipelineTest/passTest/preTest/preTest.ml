open OUnit2

let suite =
  "Preprocessing" >::: [
    TokenTest.suite;
    LexerTest.suite;
  ]
