(** {1 Parse Tree} *)

(** {2 Data Types} *)

type top
(** A top-level statement *)

type file = private
  | File of {
      tops: top list (** The top-level statements in the file *)
    } (** A parsed file *)
(** A parsed SystemVerilog file *)

(** {2 Constructors} *)

val file : top list -> file
(** [file tops] constructs a file from the list of top-level statements [tops]. *)
