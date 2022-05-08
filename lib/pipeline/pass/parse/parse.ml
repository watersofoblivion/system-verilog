(* Parsing *)

module Menhir = Parser.MenhirInterpreter

include Lexer
include Env

(* Tokenizers *)

let main_tokenizer = Sedlexing.with_tokenizer Lexer.lex_main

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

let file_pos fname = {
  Lexing.pos_fname = fname;
  Lexing.pos_lnum = 1;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}

let file path =
  let lexbuf = Lexer.lexbuf_from_file path in
  path
    |> Fpath.to_string
    |> file_pos
    |> Parser.Incremental.file
    |> parse_main lexbuf
