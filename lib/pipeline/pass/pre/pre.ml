(* Preprocessor *)

module Menhir = Parser.MenhirInterpreter

include Tokens
include Lexer
include Env

(* Tokenizers *)

let main_tokenizer = Sedlexing.with_tokenizer Lexer.lex_src

(* Higher-order Helpers *)

let input_needed tokenize parse lexbuf checkpoint =
  ()
    |> tokenize lexbuf
    |> Menhir.offer checkpoint
    |> parse lexbuf

let resume parse lexbuf checkpoint =
  checkpoint
    |> Menhir.resume
    |> parse lexbuf

(* Errors *)

exception ParseError (* of Loc.t *)
exception Rejected of string

let parse_error msg env = match Menhir.top env with
  | None ->
    Format.sprintf "Boom: %s parse_error" msg
      |> failwith
  | Some (Menhir.Element (_, _, (* start_loc *) _, (* end_loc *) _)) ->
    (* let loc = Loc.loc start_loc end_loc in
    Format.eprintf "%s parse error %@ (%d, %d, %d) - (%d, %d, %d)\n%!"
      msg
      start_loc.Lexing.pos_lnum
      (start_loc.Lexing.pos_cnum - start_loc.Lexing.pos_bol)
      start_loc.Lexing.pos_cnum
      end_loc.Lexing.pos_lnum
      (end_loc.Lexing.pos_cnum - end_loc.Lexing.pos_bol)
      end_loc.Lexing.pos_cnum
      ; *)
    ParseError (* loc *)
      |> raise

let reject msg =
  Rejected msg
    |> raise

(* Parser Drivers *)

let rec parse_main lexbuf checkpoint = match checkpoint with
  | Menhir.InputNeeded _ -> input_main lexbuf checkpoint
  | Menhir.Shifting (_, env, _) ->
    let resume = match Menhir.top env with
      | None -> failwith "Boom: parse_main no top"
      | Some (Menhir.Element (state, _, _, _)) ->
        match Menhir.incoming_symbol state with
          (* | Menhir.T Menhir.T_FROM -> resume_import_path
          | Menhir.T Menhir.T_SQUOTE -> resume_rune
          | Menhir.T Menhir.T_DQUOTE -> resume_str *)
          | Menhir.T _ -> resume_main
          | Menhir.N _ -> failwith "Expected non-terminal"
    in
    resume lexbuf checkpoint
  | Menhir.AboutToReduce _ -> resume_main lexbuf checkpoint
  | Menhir.HandlingError env -> parse_error "main" env
  | Menhir.Accepted value -> value
  | Menhir.Rejected -> reject "main"

(* Input Helpers *)

and input_main lexbuf checkpoint = input_needed main_tokenizer parse_main lexbuf checkpoint

(* Resume Helpers *)

and resume_main lexbuf checkpoint = resume parse_main lexbuf checkpoint

(* Parsers *)

(* Entry Points *)

let file_pos fname = {
  Lexing.pos_fname = fname;
  Lexing.pos_lnum = 1;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}

let parse_file path =
  let lexbuf = Lexer.lexbuf_of_file path in
  path
    |> Fpath.to_string
    |> file_pos
    |> Parser.Incremental.file
    |> parse_main lexbuf

(* Unit Test Entry Points *)

let start_pos = {
  Lexing.pos_fname = "-";
  Lexing.pos_lnum = 1;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}

let parse_ident lexbuf =
  start_pos
    |> Parser.Incremental.ident_test
    |> parse_main lexbuf

let parse_lit_num lexbuf =
  start_pos
    |> Parser.Incremental.lit_num_test
    |> parse_main lexbuf

let parse_lit_str lexbuf =
  start_pos
    |> Parser.Incremental.lit_str_test
    |> parse_main lexbuf

let parse_params lexbuf =
  start_pos
    |> Parser.Incremental.params_test
    |> parse_main lexbuf

let parse_param lexbuf =
  start_pos
    |> Parser.Incremental.param_test
    |> parse_main lexbuf

let parse_elem lexbuf =
  start_pos
    |> Parser.Incremental.elem_test
    |> parse_main lexbuf

let parse_line lexbuf =
  start_pos
    |> Parser.Incremental.line_test
    |> parse_main lexbuf

let parse_body lexbuf =
  start_pos
    |> Parser.Incremental.body_test
    |> parse_main lexbuf

let parse_args lexbuf =
  start_pos
    |> Parser.Incremental.args_test
    |> parse_main lexbuf

let parse_incl lexbuf =
  start_pos
    |> Parser.Incremental.incl_test
    |> parse_main lexbuf

let parse_pragma_expr lexbuf =
  start_pos
    |> Parser.Incremental.pragma_expr_test
    |> parse_main lexbuf

let parse_pragma_value lexbuf =
  start_pos
    |> Parser.Incremental.pragma_value_test
    |> parse_main lexbuf

let parse_dir lexbuf =
  start_pos
    |> Parser.Incremental.dir_test
    |> parse_main lexbuf

let parse_seg lexbuf =
  start_pos
    |> Parser.Incremental.seg_test
    |> parse_main lexbuf
