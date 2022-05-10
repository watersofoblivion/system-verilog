(* Initializers *)

let lexbuf_from_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  let pos = {
    Lexing.pos_fname = "-";
    Lexing.pos_lnum = 1;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = 0;
  } in
  Sedlexing.set_position lexbuf pos;
  lexbuf

let lexbuf_from_file path =
  let path = Fpath.to_string path in
  let ic = open_in path in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  let pos = {
    Lexing.pos_fname = path;
    Lexing.pos_lnum = 1;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = 0;
  } in
  Sedlexing.set_position lexbuf pos;
  Sedlexing.set_filename lexbuf path;
  lexbuf

(* Tokens *)

(* Non-Printable *)

let eof = Tokens.EOF
let newline = Tokens.NEWLINE

(* Punctuation *)

let punct_dquote = Tokens.PUNCT_DQUOTE
let punct_langle = Tokens.PUNCT_LANGLE
let punct_rangle = Tokens.PUNCT_RANGLE
let punct_lparen = Tokens.PUNCT_LPAREN
let punct_rparen = Tokens.PUNCT_RPAREN
let punct_lbracket = Tokens.PUNCT_LBRACKET
let punct_rbracket = Tokens.PUNCT_RBRACKET
let punct_lbrace = Tokens.PUNCT_LBRACE
let punct_rbrace = Tokens.PUNCT_RBRACE
let punct_comma = Tokens.PUNCT_COMMA
let punct_eq = Tokens.PUNCT_EQ
let punct_slash = Tokens.PUNCT_SLASH

(* Source Code *)

let source src = Tokens.SOURCE src
let source_of_buf buf =
  buf
    |> Buffer.contents
    |> source

(* Directives *)

let dir_reset_all = Tokens.DIR_RESET_ALL
let dir_include = Tokens.DIR_INCLUDE
let dir_define = Tokens.DIR_DEFINE
let dir_undef = Tokens.DIR_UNDEF
let dir_undefine_all = Tokens.DIR_UNDEFINE_ALL
let dir_macro name = Tokens.DIR_MACRO name
let dir_if_def = Tokens.DIR_IF_DEF
let dir_if_n_def = Tokens.DIR_IF_N_DEF
let dir_els_if = Tokens.DIR_ELS_IF
let dir_else = Tokens.DIR_ELSE
let dir_end_if = Tokens.DIR_END_IF
let dir_timescale = Tokens.DIR_TIMESCALE
let dir_default_net_type = Tokens.DIR_DEFAULT_NET_TYPE
let dir_unconnected_drive = Tokens.DIR_UNCONNECTED_DRIVE
let dir_no_unconnected_drive = Tokens.DIR_NO_UNCONNECTED_DRIVE
let dir_cell_define = Tokens.DIR_CELL_DEFINE
let dir_end_cell_define = Tokens.DIR_END_CELL_DEFINE
let dir_pragma = Tokens.DIR_PRAGMA
let dir_line = Tokens.DIR_LINE_LEVEL
let dir_FILE = Tokens.DIR_FILE
let dir_LINE = Tokens.DIR_LINE
let dir_begin_keywords = Tokens.DIR_BEGIN_KEYWORDS
let dir_end_keywords = Tokens.DIR_END_KEYWORDS

let dir id =
  try
    List.assoc id [
      ("resetall", dir_reset_all);
      ("include", dir_include);
      ("define", dir_define);
      ("undef", dir_undef);
      ("undefineall", dir_undefine_all);
      ("ifdef", dir_if_def);
      ("ifndef", dir_if_n_def);
      ("elsif", dir_els_if);
      ("else", dir_else);
      ("endif", dir_end_if);
      ("timescale", dir_timescale);
      ("default_nettype", dir_default_net_type);
      ("unconnected_drive", dir_unconnected_drive);
      ("nounconnected_drive", dir_no_unconnected_drive);
      ("celldefine", dir_cell_define);
      ("endcelldefine", dir_end_cell_define);
      ("pragma", dir_pragma);
      ("line", dir_line);
      ("__FILE__", dir_FILE);
      ("__LINE__", dir_LINE);
      ("begin_keywords", dir_begin_keywords);
      ("end_keywords", dir_end_keywords);
    ]
  with Not_found ->
    dir_macro id

(* Net Types *)

let net_wire = Tokens.NET_TYPE_WIRE
let net_tri = Tokens.NET_TYPE_TRI
let net_tri_0 = Tokens.NET_TYPE_TRI_0
let net_tri_1 = Tokens.NET_TYPE_TRI_1
let net_w_and = Tokens.NET_TYPE_W_AND
let net_tri_and = Tokens.NET_TYPE_TRI_AND
let net_w_or = Tokens.NET_TYPE_W_OR
let net_tri_or = Tokens.NET_TYPE_TRI_OR
let net_tri_reg = Tokens.NET_TYPE_TRI_REG
let net_u_wire = Tokens.NET_TYPE_U_WIRE
let net_none = Tokens.NET_TYPE_NONE

let net nettype =
  List.assoc nettype [
    ("wire", net_wire);
    ("tri", net_tri);
    ("tri0", net_tri_0);
    ("tri1", net_tri_1);
    ("wand", net_w_and);
    ("triand", net_tri_and);
    ("wor", net_w_or);
    ("trior", net_tri_or);
    ("trireg", net_tri_reg);
    ("uwire", net_u_wire);
    ("none", net_none);
  ]

(* Lexers *)

(* Source Code *)

let add lexbuf buf =
  lexbuf
    |> Sedlexing.Utf8.lexeme
    |> Buffer.add_string buf;
  lexbuf

let rec lex_src lexbuf = real_lex_src (Buffer.create 100) lexbuf
and real_lex_src buf lexbuf =
  match%sedlex lexbuf with
    (* Non-printable *)
    | eof ->
      if Buffer.length buf = 0
      then eof
      else source_of_buf buf

    (* Comments *)
    | "//" ->
      buf
        |> add lexbuf
        |> lex_src_single_line_comment buf
    | "/*" ->
      buf
        |> add lexbuf
        |> lex_src_multi_line_comment buf

    (* Strings *)
    | '"' ->
      buf
        |> add lexbuf
        |> lex_src_string buf

    (* Compiler Directives *)
    | '`' -> source_of_buf buf

    (* Everything Else *)
    | Compl '`' ->
      buf
        |> add lexbuf
        |> real_lex_src buf
    | _ -> failwith "Un-possible!!!"

and lex_src_single_line_comment buf lexbuf =
  match%sedlex lexbuf with
    | '\n' ->
      buf
        |> add lexbuf
        |> real_lex_src buf
    | eof -> source_of_buf buf
    | any ->
      buf
        |> add lexbuf
        |> lex_src_single_line_comment buf
    | _ -> failwith "Un-possible!!!"

and lex_src_multi_line_comment buf lexbuf =
  match%sedlex lexbuf with
    | eof   -> source_of_buf buf
    | "\\*" ->
      Buffer.add_string buf "\\*";
      lex_src_multi_line_comment buf lexbuf
    | "*/" ->
      buf
        |> add lexbuf
        |> real_lex_src buf
    | any ->
      buf
        |> add lexbuf
        |> lex_src_multi_line_comment buf
    | _ -> failwith "Un-possible!!!"

and lex_src_string buf lexbuf =
  match%sedlex lexbuf with
    | eof   -> source_of_buf buf
    | "\\\"" ->
      buf
        |> add lexbuf
        |> lex_src_string buf
    | '"' ->
      buf
        |> add lexbuf
        |> real_lex_src buf
    | any ->
      buf
        |> add lexbuf
        |> lex_src_string buf
    | _ -> failwith "Un-possible!!!"

(* Directives *)

let lex_dir lexbuf =
  match%sedlex lexbuf with
    | eof -> eof
    | Star '_', (uppercase|lowercase), Star (uppercase|lowercase|hex_digit|'_') ->
      lexbuf
        |> Sedlexing.Utf8.lexeme
        |> dir
    | _ -> failwith "Un-possible!!!"
