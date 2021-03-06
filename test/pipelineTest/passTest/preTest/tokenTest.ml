(* Lexer *)

open OUnit2

(* Utilites *)

let not_equal _ _ = false

let token_printer = function
  (* Non-Printable *)
  | Pre.EOF -> "<eof>"
  | Pre.NEWLINE -> "\\n"

  (* Punctuation *)
  | Pre.PUNCT_DQUOTE -> "\""
  | Pre.PUNCT_LANGLE -> "\"<\""
  | Pre.PUNCT_RANGLE -> "\">\""
  | Pre.PUNCT_LPAREN -> "\"(\""
  | Pre.PUNCT_RPAREN -> "\")\""
  | Pre.PUNCT_LBRACKET -> "\"[\""
  | Pre.PUNCT_RBRACKET -> "\"]\""
  | Pre.PUNCT_LBRACE -> "\"{\""
  | Pre.PUNCT_RBRACE -> "\"}\""
  | Pre.PUNCT_COMMA -> "\",\""
  | Pre.PUNCT_EQ -> "\"=\""
  | Pre.PUNCT_SLASH -> "\"/\""

  (* Source Code *)
  | Pre.SOURCE src -> Printf.sprintf "(source %S)" src

  (* Directives *)
  | Pre.DIR_RESET_ALL -> "(directive resetall)"
  | Pre.DIR_INCLUDE -> "(directive include)"
  | Pre.DIR_DEFINE -> "(directive define)"
  | Pre.DIR_UNDEF -> "(directive undef)"
  | Pre.DIR_UNDEFINE_ALL -> "(directive undefineall)"
  | Pre.DIR_MACRO id -> Printf.sprintf "(macro %S)" id
  | Pre.DIR_IF_DEF -> "(directive ifdef)"
  | Pre.DIR_IF_N_DEF -> "(directive ifndef)"
  | Pre.DIR_ELS_IF -> "(directive elsif)"
  | Pre.DIR_ELSE -> "(directive else)"
  | Pre.DIR_END_IF -> "(directive endif)"
  | Pre.DIR_TIMESCALE -> "(directive timescale)"
  | Pre.DIR_DEFAULT_NET_TYPE -> "(directive default_nettype)"
  | Pre.DIR_UNCONNECTED_DRIVE -> "(directive unconnected_drive)"
  | Pre.DIR_NO_UNCONNECTED_DRIVE -> "(directive nounconnected_drive)"
  | Pre.DIR_CELL_DEFINE -> "(directive celldefine)"
  | Pre.DIR_END_CELL_DEFINE -> "(directive endcelldefine)"
  | Pre.DIR_PRAGMA -> "(directive pragma)"
  | Pre.DIR_LINE_LEVEL -> "(directive line)"
  | Pre.DIR_FILE -> "(directive __FILE__)"
  | Pre.DIR_LINE -> "(directive __LINE__)"
  | Pre.DIR_BEGIN_KEYWORDS -> "(directive begin_keywords)"
  | Pre.DIR_END_KEYWORDS -> "(directive end_keywords)"

  (* Net Types *)
  | Pre.NUMBER lexeme -> Printf.sprintf "(number %S)" lexeme
  | Pre.IDENT lexeme -> Printf.sprintf "(identifier %S)" lexeme

let assert_token_equal ~ctxt expected actual = match expected, actual with
  (* Non-Printable *)
  | Pre.EOF, Pre.EOF
  | Pre.NEWLINE, Pre.NEWLINE -> ()

  (* Punctuation *)
  | Pre.PUNCT_DQUOTE, Pre.PUNCT_DQUOTE
  | Pre.PUNCT_LANGLE, Pre.PUNCT_LANGLE
  | Pre.PUNCT_RANGLE, Pre.PUNCT_RANGLE
  | Pre.PUNCT_LPAREN, Pre.PUNCT_LPAREN
  | Pre.PUNCT_RPAREN, Pre.PUNCT_RPAREN
  | Pre.PUNCT_LBRACKET, Pre.PUNCT_LBRACKET
  | Pre.PUNCT_RBRACKET, Pre.PUNCT_RBRACKET
  | Pre.PUNCT_LBRACE, Pre.PUNCT_LBRACE
  | Pre.PUNCT_RBRACE, Pre.PUNCT_RBRACE
  | Pre.PUNCT_COMMA, Pre.PUNCT_COMMA
  | Pre.PUNCT_EQ, Pre.PUNCT_EQ
  | Pre.PUNCT_SLASH, Pre.PUNCT_SLASH -> ()

  (* Source Code *)
  | Pre.SOURCE expected, Pre.SOURCE actual ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Sources are not equal" expected actual

  (* Directives *)
  | Pre.DIR_RESET_ALL, Pre.DIR_RESET_ALL
  | Pre.DIR_INCLUDE, Pre.DIR_INCLUDE
  | Pre.DIR_DEFINE, Pre.DIR_DEFINE
  | Pre.DIR_UNDEF, Pre.DIR_UNDEF
  | Pre.DIR_UNDEFINE_ALL, Pre.DIR_UNDEFINE_ALL -> ()
  | Pre.DIR_MACRO expected, Pre.DIR_MACRO actual ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Macros are not equal" expected actual
  | Pre.DIR_IF_DEF, Pre.DIR_IF_DEF
  | Pre.DIR_IF_N_DEF, Pre.DIR_IF_N_DEF
  | Pre.DIR_ELS_IF, Pre.DIR_ELS_IF
  | Pre.DIR_ELSE, Pre.DIR_ELSE
  | Pre.DIR_END_IF, Pre.DIR_END_IF
  | Pre.DIR_TIMESCALE, Pre.DIR_TIMESCALE
  | Pre.DIR_DEFAULT_NET_TYPE, Pre.DIR_DEFAULT_NET_TYPE
  | Pre.DIR_UNCONNECTED_DRIVE, Pre.DIR_UNCONNECTED_DRIVE
  | Pre.DIR_NO_UNCONNECTED_DRIVE, Pre.DIR_NO_UNCONNECTED_DRIVE
  | Pre.DIR_CELL_DEFINE, Pre.DIR_CELL_DEFINE
  | Pre.DIR_END_CELL_DEFINE, Pre.DIR_END_CELL_DEFINE
  | Pre.DIR_PRAGMA, Pre.DIR_PRAGMA
  | Pre.DIR_LINE_LEVEL, Pre.DIR_LINE_LEVEL
  | Pre.DIR_FILE, Pre.DIR_FILE
  | Pre.DIR_LINE, Pre.DIR_LINE
  | Pre.DIR_BEGIN_KEYWORDS, Pre.DIR_BEGIN_KEYWORDS
  | Pre.DIR_END_KEYWORDS, Pre.DIR_END_KEYWORDS -> ()

  (* General *)
  | Pre.NUMBER expected, Pre.NUMBER actual ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Numeric lexemes are not equal" expected actual
  | Pre.IDENT expected, Pre.IDENT actual ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Identifier lexemes are not equal" expected actual

  (* Failure *)
  | _ -> assert_equal ~ctxt ~cmp:not_equal ~printer:token_printer ~msg:"Tokens are not equal" expected actual

(* Tokens *)

let test_eof _ =
  match Pre.eof with
    | Pre.EOF -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_newline _ =
  match Pre.newline with
    | Pre.NEWLINE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_dquote _ =
  match Pre.punct_dquote with
    | Pre.PUNCT_DQUOTE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_langle _ =
  match Pre.punct_langle with
    | Pre.PUNCT_LANGLE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_rangle _ =
  match Pre.punct_rangle with
    | Pre.PUNCT_RANGLE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_lparen _ =
  match Pre.punct_lparen with
    | Pre.PUNCT_LPAREN -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_rparen _ =
  match Pre.punct_rparen with
    | Pre.PUNCT_RPAREN -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_lbracket _ =
  match Pre.punct_lbracket with
    | Pre.PUNCT_LBRACKET -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_rbracket _ =
  match Pre.punct_rbracket with
    | Pre.PUNCT_RBRACKET -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_lbrace _ =
  match Pre.punct_lbrace with
    | Pre.PUNCT_LBRACE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_rbrace _ =
  match Pre.punct_rbrace with
    | Pre.PUNCT_RBRACE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_comma _ =
  match Pre.punct_comma with
    | Pre.PUNCT_COMMA -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_eq _ =
  match Pre.punct_eq with
    | Pre.PUNCT_EQ -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_punct_slash _ =
  match Pre.punct_slash with
    | Pre.PUNCT_SLASH -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_source ctxt =
  let src = "the source" in
  match Pre.source src with
    | Pre.SOURCE actual -> assert_equal ~ctxt ~printer:Fun.id ~msg:"Sources are not equal" src actual
    | _ -> assert_failure "Tokens are not equal"

let test_dir_reset_all _ =
  match Pre.dir_reset_all with
    | Pre.DIR_RESET_ALL -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_include _ =
  match Pre.dir_include with
    | Pre.DIR_INCLUDE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_define _ =
  match Pre.dir_define with
    | Pre.DIR_DEFINE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_undef _ =
  match Pre.dir_undef with
    | Pre.DIR_UNDEF -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_undefine_all _ =
  match Pre.dir_undefine_all with
    | Pre.DIR_UNDEFINE_ALL -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_macro ctxt =
  let macro = "themacro" in
  match Pre.dir_macro macro with
    | Pre.DIR_MACRO actual -> assert_equal ~ctxt ~printer:Fun.id ~msg:"Macros are not equal" macro actual
    | _ -> assert_failure "Tokens are not equal"

let test_dir_if_def _ =
  match Pre.dir_if_def with
    | Pre.DIR_IF_DEF -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_if_n_def _ =
  match Pre.dir_if_n_def with
    | Pre.DIR_IF_N_DEF -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_els_if _ =
  match Pre.dir_els_if with
    | Pre.DIR_ELS_IF -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_else _ =
  match Pre.dir_else with
    | Pre.DIR_ELSE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_end_if _ =
  match Pre.dir_end_if with
    | Pre.DIR_END_IF -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_timescale _ =
  match Pre.dir_timescale with
    | Pre.DIR_TIMESCALE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_default_net_type _ =
  match Pre.dir_default_net_type with
    | Pre.DIR_DEFAULT_NET_TYPE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_unconnected_drive _ =
  match Pre.dir_unconnected_drive with
    | Pre.DIR_UNCONNECTED_DRIVE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_no_unconnected_drive _ =
  match Pre.dir_no_unconnected_drive with
    | Pre.DIR_NO_UNCONNECTED_DRIVE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_cell_define _ =
  match Pre.dir_cell_define with
    | Pre.DIR_CELL_DEFINE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_end_cell_define _ =
  match Pre.dir_end_cell_define with
    | Pre.DIR_END_CELL_DEFINE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_pragma _ =
  match Pre.dir_pragma with
    | Pre.DIR_PRAGMA -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_line _ =
  match Pre.dir_line with
    | Pre.DIR_LINE_LEVEL -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_FILE _ =
  match Pre.dir_FILE with
    | Pre.DIR_FILE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_LINE _ =
  match Pre.dir_LINE with
    | Pre.DIR_LINE -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_begin_keywords _ =
  match Pre.dir_begin_keywords with
    | Pre.DIR_BEGIN_KEYWORDS -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir_end_keywords _ =
  match Pre.dir_end_keywords with
    | Pre.DIR_END_KEYWORDS -> ()
    | _ -> assert_failure "Tokens are not equal"

let test_dir ctxt =
  let assert_dir (name, expected) =
    let expected = match expected with
      | Some expected -> expected
      | None -> Pre.dir_macro name
    in
    name
      |> Pre.dir
      |> assert_token_equal ~ctxt expected
  in
  List.iter assert_dir [
    ("resetall", Some Pre.dir_reset_all);
    ("include", Some Pre.dir_include);
    ("define", Some Pre.dir_define);
    ("undef", Some Pre.dir_undef);
    ("undefineall", Some Pre.dir_undefine_all);
    ("ifdef", Some Pre.dir_if_def);
    ("ifndef", Some Pre.dir_if_n_def);
    ("elsif", Some Pre.dir_els_if);
    ("else", Some Pre.dir_else);
    ("endif", Some Pre.dir_end_if);
    ("timescale", Some Pre.dir_timescale);
    ("default_nettype", Some Pre.dir_default_net_type);
    ("unconnected_drive", Some Pre.dir_unconnected_drive);
    ("nounconnected_drive", Some Pre.dir_no_unconnected_drive);
    ("celldefine", Some Pre.dir_cell_define);
    ("endcelldefine", Some Pre.dir_end_cell_define);
    ("pragma", Some Pre.dir_pragma);
    ("line", Some Pre.dir_line);
    ("__FILE__", Some Pre.dir_FILE);
    ("__LINE__", Some Pre.dir_LINE);
    ("begin_keywords", Some Pre.dir_begin_keywords);
    ("end_keywords", Some Pre.dir_end_keywords);
  ]

(* General *)

let test_number ctxt =
  let lexeme = "42" in
  match Pre.number lexeme with
    | Pre.NUMBER actual -> assert_equal ~ctxt ~printer:Fun.id ~msg:"Numeric lexemes are not equal" lexeme actual
    | _ -> assert_failure "Tokens are not equal"

let test_ident ctxt =
  let lexeme = "42" in
  match Pre.ident lexeme with
    | Pre.IDENT actual -> assert_equal ~ctxt ~printer:Fun.id ~msg:"Identifier lexemes are not equal" lexeme actual
    | _ -> assert_failure "Tokens are not equal"

(* Test Suite *)

let suite =
  "Tokens" >::: [
    "Punctuation" >::: [
      "Non-Printable" >::: [
        "End-of-File" >:: test_eof;
        "New Line"    >:: test_newline;
      ];
      "Double Quote" >:: test_punct_dquote;
      "Angle Brackets" >::: [
        "Left"  >:: test_punct_langle;
        "Right" >:: test_punct_rangle;
      ];
      "Parenthesis" >::: [
        "Left"  >:: test_punct_lparen;
        "Right" >:: test_punct_rparen;
      ];
      "Square Brackets" >::: [
        "Left"  >:: test_punct_lbracket;
        "Right" >:: test_punct_rbracket;
      ];
      "Curly Braces" >::: [
        "Left"  >:: test_punct_lbrace;
        "Right" >:: test_punct_rbrace;
      ];
      "Comma"  >:: test_punct_comma;
      "Equals" >:: test_punct_eq;
      "Slash"  >:: test_punct_slash;
    ];
    "Source" >:: test_source;
    "Directives" >::: [
      "Reset All" >:: test_dir_reset_all;
      "Include"   >:: test_dir_include;
      "Macro Definitions" >::: [
        "Define"       >:: test_dir_define;
        "Undefine"     >:: test_dir_undef;
        "Undefine All" >:: test_dir_undefine_all;
        "Expansion"    >:: test_dir_macro;
      ];
      "Conditional Compilation" >::: [
        "If Defined"     >:: test_dir_if_def;
        "If Not Defined" >:: test_dir_if_n_def;
        "Else If"        >:: test_dir_els_if;
        "Else"           >:: test_dir_else;
        "End If"         >:: test_dir_end_if;
      ];
      "Timescales"        >:: test_dir_timescale;
      "Default Net Types" >:: test_dir_default_net_type;
      "Drive Unconnected Pins" >::: [
        "Begin" >:: test_dir_unconnected_drive;
        "End"   >:: test_dir_no_unconnected_drive;
      ];
      "Cell Modules" >::: [
        "Begin" >:: test_dir_cell_define;
        "End"   >:: test_dir_end_cell_define;
      ];
      "Pragma"     >:: test_dir_pragma;
      "Line Level" >:: test_dir_line;
      "Source Locations" >::: [
        "Current File Name"   >:: test_dir_FILE;
        "Current Line Number" >:: test_dir_LINE;
      ];
      "Keywords" >::: [
        "Begin" >:: test_dir_begin_keywords;
        "End"   >:: test_dir_end_keywords;
      ];
      "Generic" >:: test_dir;
    ];
    "Generic" >::: [
      "Numbers"     >:: test_number;
      "Identifiers" >:: test_ident;
    ];
  ]
