open OUnit2

let suite =
  "Languages" >::: [
    CommTest.suite;
    PostTest.suite;
    SyntaxTest.suite;
    AstTest.suite;
  ]
