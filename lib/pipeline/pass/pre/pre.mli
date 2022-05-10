(** {1 Preprocessor} *)

(** {2 Tokens} *)

type token = private
  | SOURCE of string
  | PUNCT_SLASH
  | PUNCT_RPAREN
  | PUNCT_LT
  | PUNCT_LPAREN
  | PUNCT_GT
  | PUNCT_EQ
  | PUNCT_DQUOTE
  | PUNCT_COMMA
  | NEWLINE
  | NET_TYPE_W_OR
  | NET_TYPE_W_AND
  | NET_TYPE_WIRE
  | NET_TYPE_U_WIRE
  | NET_TYPE_TRI_REG
  | NET_TYPE_TRI_OR
  | NET_TYPE_TRI_AND
  | NET_TYPE_TRI_1
  | NET_TYPE_TRI_0
  | NET_TYPE_TRI
  | NET_TYPE_NONE
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
(** Tokens *)

(** {3 Non-Printable} *)

val eof : token
(** [eof] constructs an end-of-file token. *)

(** {3 Punctuation} *)

val punct_dquote : token
(** [punct_dquote] constructs a double quote punctuation token. *)

val punct_lt : token
(** [punct_lt] constructs a less than punctuation token. *)

val punct_gt : token
(** [punct_gt] constructs a greater than punctuation token. *)

val punct_lparen : token
(** [punct_lparen] constructs a left parenthesis punctuation token. *)

val punct_rparen : token
(** [punct_rparen] constructs a right parenthesis punctuation token. *)

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

(** {3 Net Types} *)

val net : string -> token
(** [dir lexeme] constructs a net type from the contents of the lexeme [lexeme]. *)

val net_wire : token
(** [net_wire] constructs a wire net type token. *)

val net_tri : token
(** [net_tri] constructs a tri net type token. *)

val net_tri_0 : token
(** [net_tri_0] constructs a tri0 net type token. *)

val net_tri_1 : token
(** [net_tri_1] constructs a tri1 net type token. *)

val net_w_and : token
(** [net_w_and] constructs a wand net type token. *)

val net_tri_and : token
(** [net_tri_and] constructs a triand net type token. *)

val net_w_or : token
(** [net_w_or] constructs a wor net type token. *)

val net_tri_or : token
(** [net_tri_or] constructs a trior net type token. *)

val net_tri_reg : token
(** [net_tri_reg] constructs a trireg net type token. *)

val net_u_wire : token
(** [net_u_wire] constructs a uwire net type token. *)

val net_none : token
(** [net_none] constructs a none net type token. *)

(** {2 Lexing Buffers} *)

val lexbuf_from_string : string -> Sedlexing.lexbuf
(** [lexbuf_from_string src] constructs a lexing buffer from the string [src]. *)

val lexbuf_from_file : Fpath.t -> Sedlexing.lexbuf
(** [lexbuf_from_file path] constructs a lexing buffer reading from the file
    [path]. *)

(** {2 Lexers} *)

val lex_src : Sedlexing.lexbuf -> token
(** [lex_src buf lexbuf] lexes source code from the lexing buffer [lexbuf] until
    a compiler directive is encountered. *)

val lex_dir : Sedlexing.lexbuf -> token
(** [lex_dir buf lexbuf] lexes a directive or macro name. *)

(** {2 Environments} *)

type env
(** A parsing environment *)

val empty : env
(** [empty] constructs an empty parsing environment. *)

(** {2 Entry Points} *)

val compilation_unit : Fpath.t -> env -> (env -> Post.t -> 'a) -> 'a
(** [compilation_unit path env kontinue] parses the file located at path [path]
    in the environment [env] and passes an environment and the parsed file to
    the continuation [kontinue]. *)

(** {2 Unit Test Entry Points} *)
