(** {1 Preprocessor} *)

(**
 * {2 Lexers}
 *)

val lexbuf_from_string : string -> Sedlexing.lexbuf
(** [lexbuf_from_string src] constructs a lexing buffer from the string [src]. *)

val lexbuf_from_file : Fpath.t -> Sedlexing.lexbuf
(** [lexbuf_from_file path] constructs a lexing buffer reading from the file
    [path]. *)

(**
 * {2 Environments}
 *)

type env
(** A parsing environment *)

val empty : env
(** [empty] constructs an empty parsing environment. *)

(**
 * {2 Entry Points}
 *)

(* val file : Fpath.t -> env -> (env -> Syntax.file -> 'a) -> 'a *)
(** [file path env kontinue] parses the file located at path [path] in the
    environment [env] and passes an environment and the parsed file to the
    continuation [kontinue]. *)

(**
 * {2 Unit Test Entry Points}
 *)
