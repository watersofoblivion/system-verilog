(** {1 Preprocessor} *)

(** {2 Tokens} *)

type token = private
  | STRING
  | SOURCE of string
  | PUNCT_SLASH
  | PUNCT_RPAREN
  | PUNCT_RBRACKET
  | PUNCT_RBRACE
  | PUNCT_RANGLE
  | PUNCT_LPAREN
  | PUNCT_LBRACKET
  | PUNCT_LBRACE
  | PUNCT_LANGLE
  | PUNCT_EQ
  | PUNCT_DQUOTE
  | PUNCT_COMMA
  | NUMBER of string
  | NEWLINE
  | LINE_SEP
  | LINE of string
  | IDENT of string
  | EOF
  | DIR_UNDEFINE_ALL
  | DIR_UNDEF
  | DIR_UNCONNECTED_DRIVE
  | DIR_TIMESCALE
  | DIR_RESET_ALL
  | DIR_PRAGMA
  | DIR_NO_UNCONNECTED_DRIVE
  | DIR_MACRO of string
  | DIR_LINE_LEVEL
  | DIR_LINE
  | DIR_INCLUDE
  | DIR_IF_N_DEF
  | DIR_IF_DEF
  | DIR_FILE
  | DIR_END_KEYWORDS
  | DIR_END_IF
  | DIR_END_CELL_DEFINE
  | DIR_ELS_IF
  | DIR_ELSE
  | DIR_DEFINE
  | DIR_DEFAULT_NET_TYPE
  | DIR_CELL_DEFINE
  | DIR_BEGIN_KEYWORDS
  | DEFAULT of string
(** Tokens *)

(** {3 Non-Printable} *)

val eof : token
(** [eof] constructs an end-of-file token. *)

val newline : token
(** [newline] constructs an newline token. *)

(** {3 Punctuation} *)

val punct_dquote : token
(** [punct_dquote] constructs a double quote punctuation token. *)

val punct_langle : token
(** [punct_langle] constructs a left angle bracket punctuation token. *)

val punct_rangle : token
(** [punct_rangle] constructs a right angle bracket punctuation token. *)

val punct_lparen : token
(** [punct_lparen] constructs a left parenthesis punctuation token. *)

val punct_rparen : token
(** [punct_rparen] constructs a right parenthesis punctuation token. *)

val punct_lbracket : token
(** [punct_lbracket] constructs a left bracket punctuation token. *)

val punct_rbrace : token
(** [punct_rbrace] constructs a right brace punctuation token. *)

val punct_lbrace : token
(** [punct_lbrace] constructs a left brace punctuation token. *)

val punct_rbracket : token
(** [punct_rbracket] constructs a right bracket punctuation token. *)

val punct_comma : token
(** [punct_comma] constructs a comma punctuation token. *)

val punct_eq : token
(** [punct_eq] constructs an equal punctuation token. *)

val punct_slash : token
(** [punct_slash] constructs a slash punctuation token. *)

(** {3 Source Code} *)

val source : string -> token
(** [source buf] constructs a source token containing the source [buf]. *)

(** {3 Directives} *)

val dir : string -> token
(** [dir lexeme] constructs a directive or macro name from the contents of the
    lexeme [lexeme]. *)

val dir_reset_all : token
(** [dir_reset_all] constructs a reset all directive token. *)

val dir_include : token
(** [dir_include] constructs an include directive token. *)

val dir_define : token
(** [dir_define] constructs a define directive token. *)

val dir_undef : token
(** [dir_undef] constructs a undef directive token. *)

val dir_undefine_all : token
(** [dir_undefine_all] constructs a undefine all directive token. *)

val dir_macro : string -> token
(** [dir_macro] constructs a macro directive token. *)

val dir_if_def : token
(** [dir_if_def] constructs an if defined directive token. *)

val dir_if_n_def : token
(** [dir_if_n_def] constructs an if not defined directive token. *)

val dir_els_if : token
(** [dir_els_if] constructs an else if defined directive token. *)

val dir_else : token
(** [dir_else] constructs an else directive token. *)

val dir_end_if : token
(** [dir_end_if] constructs an end if directive token. *)

val dir_timescale : token
(** [dir_timescale] construts a timescale directive token. *)

val dir_default_net_type : token
(** [dir_default_net_type] constructs a default net type directive token. *)

val dir_unconnected_drive : token
(** [dir_unconnected_drive] constructs a drive unconnected pins directive token. *)

val dir_no_unconnected_drive : token
(** [dir_no_unconnected_drive] constructs a stop driving unconnected pins
    directive token. *)

val dir_cell_define : token
(** [dir_cell_define] constructs a begin cell modules directive token. *)

val dir_end_cell_define : token
(** [dir_end_cell_define] constructs an end cell modules directive tpken. *)

val dir_pragma : token
(** [dir_pragma] constructs a pragma directive token. *)

val dir_line : token
(** [dir_line] constructs a line level directive token. *)

val dir_FILE : token
(** [dir_FILE] constructs a current file name directive token. *)

val dir_LINE : token
(** [dir_LINE] constructs a current line number directive token. *)

val dir_begin_keywords : token
(** [dir_begin_keywords] constructs a begin keywords directive token. *)

val dir_end_keywords : token
(** [dir_end_keywords] constructs a end keywords directive token. *)

(** {3 General} *)

val number : string -> token
(** [number lexeme] constructs a number token with the lexeme [lexeme]. *)

val ident : string -> token
(** [ident lexeme] constructs an identifier token with the lexeme [lexeme]. *)

(** {2 Lexing Buffers} *)

val lexbuf_of_string : string -> Sedlexing.lexbuf
(** [lexbuf_of_string src] constructs a lexing buffer from the string [src]. *)

val lexbuf_of_file : Fpath.t -> Sedlexing.lexbuf
(** [lexbuf_of_file path] constructs a lexing buffer reading from the file
    [path]. *)

(** {2 Lexers} *)

(** {3 Source Code} *)

exception LexSourceError of {
  lexbuf: Sedlexing.lexbuf; (** The current lexing buffer *)
}
(** Raised if the lexer encounters an error lexing general source code.  This
    case should be impossible, so seeing this exception indicates a compiler
    bug. *)

exception LexSourceCommentError of {
  lexbuf: Sedlexing.lexbuf; (** The current lexing buffer *)
  multi:  bool;             (** Whether or not this was a multi-line comment (I.e., the [/* ... */] syntax.) *)
}
(** Raised if the lexer encounters an error lexing a comment in general source
    code.  This case should be impossible, so seeing this exception indicates a
    compiler bug. *)

exception LexSourceStringError of {
  lexbuf: Sedlexing.lexbuf; (** The current lexing buffer *)
}
(** Raised if the lexer encounters an error lexing a string in general source
    code.  This case should be impossible, so seeing this exception indicates a
    compiler bug. *)

val lex_src : Sedlexing.lexbuf -> token
(** [lex_src buf lexbuf] lexes source code from the lexing buffer [lexbuf] until
    a compiler directive is encountered.  Raises {!LexSourceError},
    {!LexSourceCommentError}, or {!LexSourceStringError} if the lexer encounters
    and error. *)

(** {3 Directives} *)

exception LexDirError of {
  lexbuf: Sedlexing.lexbuf (** The current lexing buffer *)
}
(** Raised if the lexer cannot recognize a valid directive or macro name. *)

val lex_dir : Sedlexing.lexbuf -> token
(** [lex_dir buf lexbuf] lexes a directive or macro name. *)

(** {2 Environments} *)

type env
(** A parsing environment *)

val empty : env
(** [empty] constructs an empty parsing environment. *)

(** {2 Entry Points} *)

val parse_file : Fpath.t -> env -> (env -> Post.file -> 'a) -> 'a
(** [parse_file path env kontinue] parses the file located at path [path]
    in the environment [env] and passes an environment and the parsed file to
    the continuation [kontinue]. *)

(** {2 Unit Test Entry Points} *)

val parse_ident : Sedlexing.lexbuf -> env -> (env -> Post.name -> 'a) -> 'a
(** [parse_ident lexbuf env kontinue] parses an identifier from [lexbuf] in the
    environment [env] and passes an environment and the parsed identifier to the
    continuation [kontinue]. *)

val parse_lit_num : Sedlexing.lexbuf -> env -> (env -> Post.value -> 'a) -> 'a
(** [parse_lit_num lexbuf env kontinue] parses an numeric literal from [lexbuf]
    in the environment [env] and passes an environment and the parsed numeric
    literal to the continuation [kontinue]. *)

val parse_lit_str : Sedlexing.lexbuf -> env -> (env -> Post.value -> 'a) -> 'a
(** [parse_lit_str lexbuf env kontinue] parses an string literal from [lexbuf]
    in the environment [env] and passes an environment and the parsed string
    literal to the continuation [kontinue]. *)

val parse_params : Sedlexing.lexbuf -> env -> (env -> Post.params -> 'a) -> 'a
(** [parse_params lexbuf env kontinue] parses a macro parameter list from
    [lexbuf] in the environment [env] and passes an environment and the parsed
    macro parameter list to the continuation [kontinue]. *)

val parse_param : Sedlexing.lexbuf -> env -> (env -> Post.param -> 'a) -> 'a
(** [parse_param lexbuf env kontinue] parses a macro parameter from [lexbuf] in
    the environment [env] and passes an environment and the parsed macro
    parameter to the continuation [kontinue]. *)

val parse_elem : Sedlexing.lexbuf -> env -> (env -> Post.elem -> 'a) -> 'a
(** [parse_elem lexbuf env kontinue] parses a macro body line element from
    [lexbuf] in the environment [env] and passes an environment and the parsed
    macro body line element to the continuation [kontinue]. *)

val parse_line : Sedlexing.lexbuf -> env -> (env -> Post.line -> 'a) -> 'a
(** [parse_line lexbuf env kontinue] parses a macro body line from [lexbuf] in
    the environment [env] and passes an environment and the parsed macro body
    line to the continuation [kontinue]. *)

val parse_body : Sedlexing.lexbuf -> env -> (env -> Post.body -> 'a) -> 'a
(** [parse_body lexbuf env kontinue] parses a macro body from [lexbuf] in the
    environment [env] and passes an environment and the parsed macro body to the
    continuation [kontinue]. *)

val parse_args : Sedlexing.lexbuf -> env -> (env -> Post.args -> 'a) -> 'a
(** [parse_args lexbuf env kontinue] parses a macro arguments list from
    [lexbuf] in the environment [env] and passes an environment and the parsed
    macro arguments list to the continuation [kontinue]. *)

val parse_incl : Sedlexing.lexbuf -> env -> (env -> Post.incl -> 'a) -> 'a
(** [parse_incl lexbuf env kontinue] parses an include source from [lexbuf] in
    the environment [env] and passes an environment and the parsed include
    source to the continuation [kontinue]. *)

val parse_pragma_value : Sedlexing.lexbuf -> env -> (env -> Post.pragma_value -> 'a) -> 'a
(** [parse_pragma_value lexbuf env kontinue] parses a pragma value from [lexbuf]
    in the environment [env] and passes an environment and the parsed pragma
    value to the continuation [kontinue]. *)

val parse_pragma_expr : Sedlexing.lexbuf -> env -> (env -> Post.pragma_expr -> 'a) -> 'a
(** [parse_pragma_expr lexbuf env kontinue] parses a pragma expression from
    [lexbuf] in the environment [env] and passes an environment and the parsed
    pragma expression to the continuation [kontinue]. *)

val parse_dir : Sedlexing.lexbuf -> env -> (env -> Post.dir -> 'a) -> 'a
(** [parse_dir lexbuf env kontinue] parses a directive from [lexbuf] in the
    environment [env] and passes an environment and the parsed directive to the
    continuation [kontinue]. *)

val parse_seg : Sedlexing.lexbuf -> env -> (env -> Post.seg -> 'a) -> 'a
(** [parse_seg lexbuf env kontinue] parses a segment from [lexbuf] in the
    environment [env] and passes an environment and the parsed directive to the
    continuation [kontinue]. *)
