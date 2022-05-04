open Parser

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

let punct_eof = EOF

(* Punctuation *)

let punct_slash = PUNCT_SLASH

(* Keywords *)

let kwd_timeunit = KWD_TIMEUNIT
let kwd_timeprecision = KWD_TIMEPRECISION

(* Lexer *)

let lex lexbuf =
  match%sedlex lexbuf with
    (* Non-printable *)
    | eof -> punct_eof
    | _   -> failwith "Lex error"
