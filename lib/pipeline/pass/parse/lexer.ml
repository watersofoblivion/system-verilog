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

let punct_eof = EOF

(* Punctuation *)

let punct_slash = PUNCT_SLASH

(* Keywords *)

let kwd_timeunit = KWD_TIMEUNIT
let kwd_timeprecision = KWD_TIMEPRECISION

(* Lexer *)

let lex_main lexbuf =
  match%sedlex lexbuf with
    (* Non-printable *)
    | eof -> punct_eof

    (* Punctuation *)
    | "/" -> punct_slash

    (* Keywords *)
    | "timeunit"      -> kwd_timeunit
    | "timeprecision" -> kwd_timeprecision

    (* Fallthrough *)
    | _   -> failwith "Lex error"
