(** {1 Post-processed Compilation Unit} *)

open Format

open Common

(** {2 Data Types} *)

type param = private
  | Param of {
      name:    string;       (** The name *)
      default: string option (** The default text *)
    }
(** Macro parameters *)

type net = private
  | Wire
  | Tri
  | Tri0
  | Tri1
  | WAnd
  | TriAnd
  | WOr
  | TriOr
  | TriReg
  | UWire
(** Net Types *)

type drive = private
  | PullUp
  | PullDown
(** How to drive unconnected ports *)

type level = private
  | Entered (** First line after an include was entered. *)
  | Exited  (** First line after an include was exited. *)
(** Line Level *)

type keywords = private
  | Ieee1800_2012 (** IEEE 1800-2012 *)
(** Supported sets of keywords *)

type dir = private
  | ResetAll (** Reset all directives *)
  | Include of {
      sys:  bool;    (** Whether this is a system-defined include file.  (I.e., [<...>] syntax was used.) *)
      path: Fpath.t; (** The path to the file *)
    } (** Include a source file *)
  | Define of {
      name:   string;        (** The name of the macro *)
      params: param list;    (** The parameters to the macro *)
      body:   string option; (** The body of the macro *)
    } (** Define a macro *)
  | Undef of {
      name: string; (** The name to undefine.  If [None], undefines all macros. *)
    } (** Undefine macros *)
  | UndefineAll
  | Macro of {
      name: string;   (** The name of the macro to expand *)
      args: seg list; (** The arguments to the macro *)
    } (** Expand a macro *)
  | IfDef of {
      macro: string; (** The macro to test for *)
    } (** If defined *)
  | IfNDef of {
      macro: string; (** The macro to test for *)
    } (** If not defined *)
  | ElsIf of {
      macro: string;
    } (** Else if defined *)
  | Else (** Condition default *)
  | EndIf (** End conditional *)
  | Timescale of {
      time_unit: Timescale.s;        (** The time unit *)
      time_prec: Timescale.s option; (** The time precision *)
    } (** Set the default time scale. *)
  | DefaultNetType of {
      net: net option; (** The net type to use as the default *)
    } (** Default Net Type *)
  | UnconnectedDrive of {
      drive: drive; (** The direction to drive unconnected ports *)
    } (** Drive Unconnected Ports *)
  | NoUnconnectedDrive (** Stop pulling unconnected ports *)
  | CellDefine (** Tag modules as cell modules *)
  | EndCellDefine (** Stop tagging modules as cell modules *)
  | Pragma of {
      name:  string;                               (** The name of the pragma *)
      exprs: (string option * string option) list; (** The pragma expressions *)
    } (** Pragmas *)
  | Line of {
      number: int;          (** Line number *)
      path:   Fpath.t;      (** Source file *)
      level:  level option; (** Line level *)
    } (** Override the current location *)
  | FILE (** The current file name *)
  | LINE (** The current line number *)
  | BeginKeywords of {
      keywords: keywords; (** The keywords to use *)
    } (** Use a specific set of keywords *)
  | EndKeywords (** Stop using a specific set of keywords *)

and seg = private
  | Source of {
      src: string; (** The source text *)
    } (** Program Source *)
  | Directive of {
      dir: dir; (** The directive *)
    } (** Compiler Directive *)
(** Segments within a file *)

type file = private
  | File of {
      path: Fpath.t;  (** The path of the file *)
      segs: seg list; (** The segments in the file *)
    }
(** Files in the compilation unit *)

type t = private
  | CompilationUnit of {
      files: file list; (** The files in the compilation unit. *)
    } (** A compilation unit *)
(** Compilation units *)

(** {2 Constructors} *)

(** {3 Macro Parameters} *)

val param : string -> string option -> param
(** [param name default] constructs a macro parameter named [name].  If
    [default] is not [None], sets default text. *)

(** {3 Net Types} *)

val net_wire : net
(** [net_wire] constructs a wire net type. *)

val net_tri : net
(** [net_tri] constructs a tri net type. *)

val net_tri0 : net
(** [net_tri0] constructs a tri0 net type. *)

val net_tri1 : net
(** [net_tri1] constructs a tri1 net type. *)

val net_w_and : net
(** [net_w_and] constructs a wand net type. *)

val net_tri_and : net
(** [net_tri_and] constructs a triand net type. *)

val net_w_or : net
(** [net_w_or] constructs a wor net type. *)

val net_tri_or : net
(** [net_tri_or] constructs a trior net type. *)

val net_tri_reg : net
(** [net_tri_reg] constructs a trireg net type. *)

val net_u_wire : net
(** [net_u_wire] constructs a uwire net type. *)

(** {3 Drive} *)

val drive_up : drive
(** [drive_up] constructs a pull up drive. *)

val drive_down : drive
(** [drive_down] constructs a pull down drive. *)

(** {3 Line Levels} *)

val level_entered : level
(** [level_entered] constructs a entered level. *)

val level_exited : level
(** [level_exited] constructs a exited level. *)

(** {3 Keywords} *)

val keywords_ieee_1800_2012 : keywords
(** [keywords_ieee_1800_2012] constructs the keyword set for IEEE 1800-2012. *)

(** {3 Directives} *)

val dir_reset_all : dir
(** [dir_reset_all] constructs a reset all directive. *)

val dir_include : bool -> Fpath.t -> dir
(** [dir_include sys path] constructs an include directive including the file at
    [path].  If [sys] is [true], then only the system paths are searched (I.e.,
    the [<...>] syntax was used.) *)

val dir_define : string -> param list -> string option -> dir
(** [dir_define name params body] constructs a define directive defining the
    macro named [name] with parameters [params].  If [body] is not [None], it is
    used as the text to expand. *)

val dir_undef : string -> dir
(** [dir_undef name] constructs an undefine directive un-defining the macro
    named [name]. *)

val dir_undefine_all : dir
(** [dir_undefine_all] constructs an undefine all directive which un-defines all
    current user-defined macros. *)

val dir_macro : string -> seg list -> dir
(** [dir_macro name args] constructs a macro expansion directive which expands
    the macro named [name] with the arguments [args]. *)

val dir_if_def : string -> dir
(** [dir_if_def macro] constructs a primary case in the condition that [macro]
    is defined. *)

val dir_if_n_def : string -> dir
(** [dir_if_n_def macro] constructs a primary case in the condition that [macro]
    is not defined. *)

val dir_els_if : string -> dir
(** [dir_els_if macro] constructs a secondary case in the condition that [macro]
    is defined. *)

val dir_else : dir
(** [dir_else] constructs an else directive marking the default case of a
    conditional. *)

val dir_end_if : dir
(** [dir_end_if] constructs an end if directive that terminates a conditional. *)

val dir_timescale : Timescale.s -> Timescale.s option -> dir
(** [dir_timescale time_unit time_prec] constructs a timescale directive with
    the time unit [time_unit] and the optional time precision [time_prec].  If
    [time_prec] is [None], [time_unit] is used as the time precision. *)

val dir_default_net_type : net option -> dir
(** [dir_default_net_type net_type] construcs a default net type directive that
    sets the default net type to [net_type]. *)

val dir_unconnected_drive : drive -> dir
(** [dir_unconnected_drive drive] constructs an unconnected drive directive that
    sets unconnected pins to drive as specified in [drive]. *)

val dir_no_unconnected_drive : dir
(** [dir_no_unconnected_drive] constructs a no unconnected drive directive that
    resets the effect of a previous unconnected drive directive. *)

val dir_cell_define : dir
(** [dir_cell_define] constructs a cell define directive that tags subsequent
    modules as cell modules. *)

val dir_end_cell_define : dir
(** [dir_end_cell_define] constructs an end cell define directive that stops
    taging subsequent modules as cell modules. *)

val dir_pragma : unit -> unit -> dir
(* TODO *)

val dir_line : int -> Fpath.t -> level option -> dir
(** [dir_line number path level] constructs a line directive that overrides the
    current program location to be line [number] in [path].  If [level] is not
    [None], it determines whether an include was just entered or exited. *)

val dir_LINE : dir
(** [dir_LINE] constructs a LINE directive that expands to the current line
    number. *)

val dir_FILE : dir
(** [dir_FILE] constructs a FILE directive that expands to the name of the
    current file. *)

val dir_begin_keywords : keywords -> dir
(** [dir_begin_keywords keywords] constructs a begin keywords directive that
    sets the set of keywords in use for the subsequent source. *)

val dir_end_keywords : dir
(** [dir_end_keywords] constructs an end keywords directive that reverts the
    set of keywords to the default for the subsequent source. *)

(** {3 Segments} *)

val seg_source : string -> seg
(** [seg_source src] constructs a segment of SystemVerilog source code. *)

val seg_directive : dir -> seg
(** [seg_directive] constructs a directive segment. *)

(** {3 Files} *)

val file : Fpath.t -> seg list -> file
(** [file path segs] constructs a file located at [path] with the segments
    [segs]. *)

(** {3 Compilation Units} *)

val compilation_unit : file list -> t
(** [compilation_unit files] constructs a compilation unit of the files [files]. *)

(** {2 Pretty Printing} *)

val pp_param : param -> formatter -> unit
(** [pp_param param fmt] pretty-prints the parameter [param] to the formatter
    [fmt]. *)

val pp_net : net option -> formatter -> unit
(** [pp_net net fmt] pretty-prints the net type [net] to the formatter [fmt]. *)

val pp_drive : drive -> formatter -> unit
(** [pp_drive drive fmt] pretty-prints the dreive direction [drive] to the
    formatter [fmt]. *)

val pp_level : level option -> formatter -> unit
(** [pp_level level fmt] pretty-prints the line level [level] to the formatter
    [fmt]. *)

val pp_keywords : keywords -> formatter -> unit
(** [pp_keywords keywords fmt] pretty-prints the named set of keywords
    [keywords] to the formatter [fmt]. *)

val pp_dir : dir -> formatter -> unit
(** [pp_dir dir fmt] pretty-prints the compiler directive [dir] to the formatter
    [fmt]. *)

val pp_seg : seg -> formatter -> unit
(** [pp_seg seg fmt] pretty-prints the file segment [seg] to the formatter
    [fmt]. *)

val pp_file : file -> formatter -> unit
(** [pp_file file fmt] pretty-prints the file [file] to the formatter [fmt]. *)

val pp : t -> formatter -> unit
(** [pp compilation_unit fmt] pretty-prints the compilation unit
    [compilation_unit] to the formatter [fmt]. *)
