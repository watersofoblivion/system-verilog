(* Parsing *)

include Lexer
include Env

let file path env kontinue =
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.file in
  let res =
    path
      |> Lexer.lexbuf_from_file
      |> Sedlexing.with_tokenizer Lexer.lex
      |> parser
  in
  res env kontinue
