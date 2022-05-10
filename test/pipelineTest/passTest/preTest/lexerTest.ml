(* Lexer *)

open OUnit2

(* Assertion *)

let assert_lexes ~ctxt lexer lines =
  let lexbuf =
    lines
      |> String.concat "\n"
      |> Pre.lexbuf_from_string
  in
  let assert_lexes expected =
    lexbuf
      |> lexer
      |> TokenTest.assert_token_equal ~ctxt expected
  in
  List.iter assert_lexes

(* Source Lexer *)

let assert_lexes_src = assert_lexes Pre.lex_src

let test_lexes_src ctxt =
  let assert_src src = assert_lexes_src ~ctxt [src] [
    Pre.source src;
    Pre.eof;
  ] in
  List.iter assert_src [
    "module foo#(BAR = 42)(input logic [31:0] baz, output logic quux);";
  ]

let test_lexes_src_comment_single ctxt =
  let assert_src src = assert_lexes_src ~ctxt [src] [
    Pre.source src;
    Pre.eof;
  ] in
  List.iter assert_src [
    "// A single line comment with an embedded `backtick\n";
    "// A single line comment with an embedded `backtick that ends with an EOF";
  ]

let test_lexes_src_comment_multi ctxt =
  let assert_src src = assert_lexes_src ~ctxt [src] [
    Pre.source src;
    Pre.eof;
  ] in
  List.iter assert_src [
    "/* A multi-line comment with an embedded `backtick */";
    "/* A multi-line comment with an embedded \\*/ and `backtick */";
    "/* A multi-line comment with an embedded `backtick that ends with an EOF";
    "/*
      * A multi-line comment with multiple lines.
      *
      * An embedded `backtick
      *
      * An embedded \\*/
      */";
  ]

let test_lexes_src_string ctxt =
  let assert_src src = assert_lexes_src ~ctxt [src] [
    Pre.source src;
    Pre.eof;
  ] in
  List.iter assert_src [
    "\"A string with an embedded `backtick\"";
    "\"A string with an embedded `backtick that ends with an EOF";
    "\"A string with an embedded `backtick and \\\"\"";
  ]

let test_lexes_src_backtick ctxt =
  assert_lexes_src ~ctxt [
    "foo`bar"
  ] [
    (*
      Yes, this is correct.

      The end of a source token signals that either an EOF or a backtick was
      encountered, so the parser will switch lexers to the directive lexer.  If
      it was a backtick, this is correct.  If it was actually an EOF, that lexer
      will immediately return EOF.
    *)
    Pre.source "foo";
    Pre.source "bar";
  ]

let test_lexes_src_eof ctxt =
  assert_lexes_src ~ctxt [
    "foo"
  ] [
    Pre.source "foo";
    Pre.eof;
    Pre.eof;
  ]

let test_lexes_src_foo ctxt =
  assert_lexes_src ~ctxt [
    "// a comment";
    "foo`bar";
  ] [
    Pre.source "// a comment\nfoo";
    Pre.source "bar";
    Pre.eof;
  ]

(* Directives *)

let assert_lexes_dir = assert_lexes Pre.lex_dir

let test_lexes_dir_reset_all ctxt =
  assert_lexes_dir ~ctxt ["resetall"] [Pre.dir_reset_all]

let test_lexes_dir_include ctxt =
  assert_lexes_dir ~ctxt ["include"] [Pre.dir_include]

let test_lexes_dir_define ctxt =
  assert_lexes_dir ~ctxt ["define"] [Pre.dir_define]

let test_lexes_dir_undef ctxt =
  assert_lexes_dir ~ctxt ["undef"] [Pre.dir_undef]

let test_lexes_dir_undefine_all ctxt =
  assert_lexes_dir ~ctxt ["undefineall"] [Pre.dir_undefine_all]

let test_lexes_dir_macro ctxt =
  let name = "macro_name" in
  assert_lexes_dir ~ctxt [name] [Pre.dir_macro name]

let test_lexes_dir_if_def ctxt =
  assert_lexes_dir ~ctxt ["ifdef"] [Pre.dir_if_def]

let test_lexes_dir_if_n_def ctxt =
  assert_lexes_dir ~ctxt ["ifndef"] [Pre.dir_if_n_def]

let test_lexes_dir_els_if ctxt =
  assert_lexes_dir ~ctxt ["elsif"] [Pre.dir_els_if]

let test_lexes_dir_else ctxt =
  assert_lexes_dir ~ctxt ["else"] [Pre.dir_else]

let test_lexes_dir_end_if ctxt =
  assert_lexes_dir ~ctxt ["endif"] [Pre.dir_end_if]

let test_lexes_dir_timescale ctxt =
  assert_lexes_dir ~ctxt ["timescale"] [Pre.dir_timescale]

let test_lexes_dir_default_net_type ctxt =
  assert_lexes_dir ~ctxt ["default_nettype"] [Pre.dir_default_net_type]

let test_lexes_dir_unconnected_drive ctxt =
  assert_lexes_dir ~ctxt ["unconnected_drive"] [Pre.dir_unconnected_drive]

let test_lexes_dir_no_unconnected_drive ctxt =
  assert_lexes_dir ~ctxt ["nounconnected_drive"] [Pre.dir_no_unconnected_drive]

let test_lexes_dir_cell_define ctxt =
  assert_lexes_dir ~ctxt ["celldefine"] [Pre.dir_cell_define]

let test_lexes_dir_end_cell_define ctxt =
  assert_lexes_dir ~ctxt ["endcelldefine"] [Pre.dir_end_cell_define]

let test_lexes_dir_pragma ctxt =
  assert_lexes_dir ~ctxt ["pragma"] [Pre.dir_pragma]

let test_lexes_dir_line ctxt =
  assert_lexes_dir ~ctxt ["line"] [Pre.dir_line]

let test_lexes_dir_FILE ctxt =
  assert_lexes_dir ~ctxt ["__FILE__"] [Pre.dir_FILE]

let test_lexes_dir_LINE ctxt =
  assert_lexes_dir ~ctxt ["__LINE__"] [Pre.dir_LINE]

let test_lexes_dir_begin_keywords ctxt =
  assert_lexes_dir ~ctxt ["begin_keywords"] [Pre.dir_begin_keywords]

let test_lexes_dir_end_keywords ctxt =
  assert_lexes_dir ~ctxt ["end_keywords"] [Pre.dir_end_keywords]

let suite =
  "Lexers" >::: [
    "Source Code" >::: [
      "System Verilog Source" >:: test_lexes_src;
      "Comments" >::: [
        "Single Line" >:: test_lexes_src_comment_single;
        "Multi-Line"  >:: test_lexes_src_comment_multi;
      ];
      "Strings" >:: test_lexes_src_string;
      "Backticks"             >:: test_lexes_src_backtick;
      "EOF"                   >:: test_lexes_src_eof;
      "Foo"                   >:: test_lexes_src_foo;
    ];
    "Directives" >::: [
      "Reset All" >:: test_lexes_dir_reset_all;
      "Include"   >:: test_lexes_dir_include;
      "Macro Definitions" >::: [
        "Define"       >:: test_lexes_dir_define;
        "Undefine"     >:: test_lexes_dir_undef;
        "Undefine All" >:: test_lexes_dir_undefine_all;
        "Expansion"    >:: test_lexes_dir_macro;
      ];
      "Conditional Compilation" >::: [
        "If Defined"     >:: test_lexes_dir_if_def;
        "If Not Defined" >:: test_lexes_dir_if_n_def;
        "Else If"        >:: test_lexes_dir_els_if;
        "Else"           >:: test_lexes_dir_else;
        "End If"         >:: test_lexes_dir_end_if;
      ];
      "Timescales"        >:: test_lexes_dir_timescale;
      "Default Net Types" >:: test_lexes_dir_default_net_type;
      "Drive Unconnected Pins" >::: [
        "Begin" >:: test_lexes_dir_unconnected_drive;
        "End"   >:: test_lexes_dir_no_unconnected_drive;
      ];
      "Cell Modules" >::: [
        "Begin" >:: test_lexes_dir_cell_define;
        "End"   >:: test_lexes_dir_end_cell_define;
      ];
      "Pragma"     >:: test_lexes_dir_pragma;
      "Line Level" >:: test_lexes_dir_line;
      "Source Locations" >::: [
        "Current File Name"   >:: test_lexes_dir_FILE;
        "Current Line Number" >:: test_lexes_dir_LINE;
      ];
      "Keywords" >::: [
        "Begin" >:: test_lexes_dir_begin_keywords;
        "End"   >:: test_lexes_dir_end_keywords;
      ];
    ];
  ]
