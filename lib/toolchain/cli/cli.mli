(** {1 Command-Line Interface} *)

open Cmdliner

(** {2 Commands} *)

val cmds : unit Cmd.t
(** [cmds] is all of the available commands. *)
