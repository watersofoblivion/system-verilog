(** {1 Post-processed Compilation Unit} *)

open Format

open Common

(** {2 Data Types} *)

type name = private
  | Name of {
      loc:  Loc.t;  (** Source location *)
      name: string; (** Name *)
    } (** A name *)
(** A name *)

type value = private
  | Value of {
      loc:   Loc.t;  (** Source location *)
      value: string; (** Value *)
    } (** A value *)
(** A value *)

type param = private
  | Param of {
      loc:     Loc.t;       (** Source location *)
      name:    name;        (** The name *)
      default: value option (** The default text *)
    }
(** Macro parameters *)

type params = private
  | Params of {
      loc:    Loc.t;      (** Source location *)
      params: param list; (** Parameters *)
    }
(** Macro parameter lists *)

type elem =
  | ElemSource of {
      loc:   Loc.t;       (** Source location *)
      value: value; (** Source code *)
    } (** Source code *)
  | ElemVar of {
      loc:  Loc.t;     (** Source location *)
      name: name; (** Variable name *)
    } (** Variable *)
(** An element of a macro body *)

type line =
  | Line of {
      loc:   Loc.t;          (** Source location *)
      elems: elem list; (** Line elements *)
   } (** A line *)
(** A line of a macro body *)

type body =
  | Body of {
      loc:   Loc.t;          (** Source location *)
      lines: line list; (** Lines *)
    } (** Body *)
(** A macro body *)

type args = private
  | Args of {
      loc:  Loc.t;    (** Source location *)
      args: value list; (** Arguments *)
    } (** Arguments **)
(** Macro arguments *)

type incl = private
  | IncludePath of {
      loc:  Loc.t; (** Source location *)
      sys:  bool;  (** Whether to only search the system paths *)
      path: value; (** Path to import *)
    } (** A path *)
  | IncludeMacro of {
      loc:  Loc.t;       (** Source location *)
      name: name;        (** Macro name *)
      args: args option; (** Macro arguments *)
    } (** A macro value *)
(** Include values *)

type mag = private
  | Magnitude1 of {
      loc: Loc.t; (** Source location *)
    } (** Zero orders of magnitude *)
  | Magnitude10 of {
      loc: Loc.t; (** Source location *)
    } (** One order of magnitude *)
  | Magnitude100 of {
      loc: Loc.t; (** Source location *)
    } (** Two orders of magnitude *)
(** Orders of magnitude *)

type yoonit = private
  | UnitS of {
      loc: Loc.t; (** Source location *)
    } (** Seconds *)
  | UnitMs of {
      loc: Loc.t; (** Source location *)
    } (** Milliseconds *)
  | UnitUs of {
      loc: Loc.t; (** Source location *)
    } (** Microseconds *)
  | UnitNs of {
      loc: Loc.t; (** Source location *)
    } (** Nanoseconds *)
  | UnitPs of {
      loc: Loc.t; (** Source location *)
    } (** Picoseconds *)
  | UnitFs of {
      loc: Loc.t; (** Source location *)
    } (** Femtoseconds *)
(** Time units *)

type scale = private
  | Scale of {
      loc:    Loc.t; (** Source location *)
      mag:    value; (** Magnitude *)
      yoonit: value; (** Time unit *)
    } (** A period of time *)
(** A period of time *)

type net = private
  | NetWire of {
      loc: Loc.t; (** Source location *)
    }
  | NetTri of {
      loc: Loc.t; (** Source location *)
    }
  | NetTri0 of {
      loc: Loc.t; (** Source location *)
    }
  | NetTri1 of {
      loc: Loc.t; (** Source location *)
    }
  | NetWAnd of {
      loc: Loc.t; (** Source location *)
    }
  | NetTriAnd of {
      loc: Loc.t; (** Source location *)
    }
  | NetWOr of {
      loc: Loc.t; (** Source location *)
    }
  | NetTriOr of {
      loc: Loc.t; (** Source location *)
    }
  | NetTriReg of {
      loc: Loc.t; (** Source location *)
    }
  | NetUWire of {
      loc: Loc.t; (** Source location *)
    }
(** Net Types *)

type drive = private
  | DriveUp of {
      loc: Loc.t; (** Source location *)
    }
  | DriveDown of {
      loc: Loc.t; (** Source location *)
    }
(** How to drive unconnected ports *)

type pragma_value = private
  | PragmaValueExprs of {
      loc:   Loc.t;            (** Source location *)
      exprs: pragma_expr list; (** Expressions *)
    } (** A list of expressions *)
  | PragmaValueNum of {
      loc:   Loc.t; (** Source location *)
      value: value; (** Value *)
    } (** A numeric value *)
  | PragmaValueString of {
      loc:   Loc.t; (** Source location *)
      value: value; (** Value *)
    } (** A string value *)
  | PragmaValueIdent of {
      loc:  Loc.t; (** Source location *)
      name: name;  (** Name *)
    } (** An identifier value *)
(** Pragma values *)

and pragma_expr = private
  | PragmaExpr of {
      loc:   Loc.t;               (** Source location *)
      kwd:   name option;         (** Keyword *)
      value: pragma_value option; (** Value *)
    } (** Pragma expression *)
(** Pragma Expressions *)

type level = private
  | LevelEntered of {
      loc: Loc.t; (** Source location *)
    } (** First line after an include was entered. *)
  | LevelExited of {
      loc: Loc.t; (** Source location *)
    }  (** First line after an include was exited. *)
(** Line Level *)

type keywords = private
  | Keywords1364_1995 of {
      loc: Loc.t; (** Source location *)
    } (** IEEE 1364-1995 *)
  | Keywords1364_2001_NoConfig of {
      loc: Loc.t; (** Source location *)
    } (** IEEE 1364-2001-noconfig *)
  | Keywords1364_2001 of {
      loc: Loc.t; (** Source location *)
    } (** IEEE 1364-2001 *)
  | Keywords1364_2005 of {
      loc: Loc.t; (** Source location *)
    } (** IEEE 1364-2005 *)
  | Keywords1800_2005 of {
      loc: Loc.t; (** Source location *)
    } (** IEEE 1800-2005 *)
  | Keywords1800_2009 of {
      loc: Loc.t; (** Source location *)
    } (** IEEE 1800-2009 *)
  | Keywords1800_2012 of {
      loc: Loc.t; (** Source location *)
    } (** IEEE 1800-2012 *)
(** Supported sets of keywords *)

type dir = private
  | DirResetAll of {
      loc: Loc.t; (** Source location *)
    } (** Reset all directives *)
  | DirInclude of {
      loc: Loc.t; (** Source location *)
      src: incl;  (** The source to include *)
    } (** Include a source file *)
  | DirDefine of {
      loc:    Loc.t;         (** Source location *)
      name:   name;          (** The name of the macro *)
      params: params option; (** The parameters to the macro *)
      body:   body option;   (** The body of the macro *)
    } (** Define a macro *)
  | DirUndef of {
      loc:  Loc.t; (** Source location *)
      name: name;  (** The name to undefine.  If [None], undefines all macros. *)
    } (** Undefine macros *)
  | DirUndefineAll of {
      loc: Loc.t; (** Source location *)
    } (** Undefine all user-defined macros *)
  | DirMacro of {
      loc:  Loc.t;       (** Source location *)
      name: name;        (** The name of the macro to expand *)
      args: args option; (** The arguments to the macro *)
    } (** Expand a macro *)
  | DirIfDef of {
      loc:   Loc.t; (** Source location *)
      macro: name;  (** The macro to test for *)
    } (** If defined *)
  | DirIfNDef of {
      loc:   Loc.t; (** Source location *)
      macro: name;  (** The macro to test against *)
    } (** If not defined *)
  | DirElsIf of {
      loc:   Loc.t; (** Source location *)
      macro: name;  (** The macro to test for *)
    } (** Else if defined *)
  | DirElse of {
      loc: Loc.t; (** Source location *)
    } (** Condition default *)
  | DirEndIf of {
      loc: Loc.t; (** Source location *)
    } (** End conditional *)
  | DirTimescale of {
      loc:    Loc.t;        (** Source location *)
      yoonit: scale;        (** The time unit *)
      prec:   scale option; (** The time precision *)
    } (** Set the default time scale. *)
  | DirDefaultNetType of {
      loc: Loc.t; (** Source location *)
      net: name;  (** The net type to use as the default *)
    } (** Default Net Type *)
  | DirUnconnectedDrive of {
      loc:   Loc.t; (** Source location *)
      drive: name;  (** The direction to drive unconnected ports *)
    } (** Drive Unconnected Ports *)
  | DirNoUnconnectedDrive of {
      loc: Loc.t; (** Source location *)
    } (** Stop pulling unconnected ports *)
  | DirCellDefine of {
      loc: Loc.t; (** Source location *)
    } (** Tag modules as cell modules *)
  | DirEndCellDefine of {
      loc: Loc.t; (** Source location *)
    } (** Stop tagging modules as cell modules *)
  | DirPragma of {
      loc:   Loc.t;            (** Source location *)
      name:  name;             (** Pragma name *)
      exprs: pragma_expr list; (** Expressions *)
    } (** Pragma *)
  | DirLine of {
      loc:    Loc.t; (** Source location *)
      number: value; (** Line number *)
      path:   value; (** Source file *)
      level:  value; (** Line level *)
    } (** Override the current location *)
  | DirFILE of {
      loc: Loc.t; (** Source location *)
    } (** The current file name *)
  | DirLINE of {
      loc: Loc.t; (** Source location *)
    } (** The current line number *)
  | DirBeginKeywords of {
      loc:      Loc.t; (** Source location *)
      keywords: value; (** The keywords to use *)
    } (** Use a specific set of keywords *)
  | DirEndKeywords of {
      loc: Loc.t; (** Source location *)
    } (** Stop using a specific set of keywords *)

type seg = private
  | SegSource of {
      loc: Loc.t;  (** Source location *)
      src: string; (** The source text *)
    } (** Program Source *)
  | SegDirective of {
      loc: Loc.t; (** Source location *)
      dir: dir;   (** The directive *)
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

(** {3 Names} *)

val name : Loc.t -> string -> name
(** [name loc name] constructs a name at location [loc] named [name]. *)

(** {3 Values} *)

val value : Loc.t -> string -> value
(** [value loc value] constructs a value at location [loc] with value [value]. *)

(** {3 Macros} *)

(** {4 Parameters} *)

val param : Loc.t -> name -> value option -> param
(** [param loc name default] constructs a macro parameter named [name] at
    location [loc].  If [default] is not [None], sets default text. *)

val params : Loc.t -> param list -> params
(** [params loc params] constructs a macro parameter list at location [loc] with
    the parameters [params]. *)

(** {4 Bodies} *)

val elem_source : Loc.t -> value -> elem
(** [elem_source loc value] constructs a source code macro body element at
    location [loc] with the source code [value]. *)

val elem_var : Loc.t -> name -> elem
(** [elem_var loc name] constructs a variable macro body element at location
    [loc] referencing the parameter named [name]. *)

val line : Loc.t -> elem list -> line
(** [line loc elems] constucts a macro body line at location [loc] composed of
    the elements [elems]. *)

val body : Loc.t -> line list -> body
(** [body loc lines] constructs a macro body at location [loc] composed of the
    lines [lines]. *)

(** {4 Arguments} *)

val args : Loc.t -> value list -> args
(** [args loc args] constructs a list of macro arguments at [loc] with the
    arguments [args]. *)

(** {3 Includes} *)

val incl_path : Loc.t -> bool -> value -> incl
(** [incl_path loc sys path] constructs an include source at location [loc]
    that includes the file at path [path].  If [sys] is [true], only the system
    paths are searched. (I.e., the [<...>] syntax was used.) *)

val incl_macro : Loc.t -> name -> args option -> incl
(** [incl_macro loc name args] constructs an include source at location [loc]
    that includes the results of expanding the macro named [name] with the
    arguments [args]. *)

(** {3 Time Scales} *)

(** {4 Orders of Magnitude} *)

val mag_1 : Loc.t -> mag
(** [mag_1 loc] constructs a zero orders of magnitude at location [loc]. *)

val mag_10 : Loc.t -> mag
(** [mag_10 loc] constructs a one order of magnitude at location [loc]. *)

val mag_100 : Loc.t -> mag
(** [mag_100 loc] constructs a two orders of magnitude at location [loc]. *)

(** {4 Time Units} *)

val unit_s : Loc.t -> yoonit
(** [unit_s loc] constructs a seconds unit at location [loc]. *)

val unit_ms : Loc.t -> yoonit
(** [unit_ms loc] constructs a millisecond unit at location [loc]. *)

val unit_us : Loc.t -> yoonit
(** [unit_us loc] constructs a microsecond unit at location [loc]. *)

val unit_ns : Loc.t -> yoonit
(** [unit_ns loc] constructs a nanosecond unit at location [loc]. *)

val unit_ps : Loc.t -> yoonit
(** [unit_ps loc] constructs a picosecond unit at location [loc]. *)

val unit_fs : Loc.t -> yoonit
(** [unit_fs loc] constructs a femtosecond unit at location [loc]. *)

(** {4 Scales} *)

val scale : Loc.t -> value -> value -> scale
(** [scale loc mag yoonit] constructs a scale of [mag] [yoonit]s at location
    [loc]. *)

(** {3 Net Types} *)

val net_wire : Loc.t -> net
(** [net_wire loc] constructs a wire net type at location [loc]. *)

val net_tri : Loc.t -> net
(** [net_tri loc] constructs a tri net type at location [loc]. *)

val net_tri0 : Loc.t -> net
(** [net_tri0 loc] constructs a tri0 net type at location [loc]. *)

val net_tri1 : Loc.t -> net
(** [net_tri1 loc] constructs a tri1 net type at location [loc]. *)

val net_w_and : Loc.t -> net
(** [net_w_and loc] constructs a wand net type at location [loc]. *)

val net_tri_and : Loc.t -> net
(** [net_tri_and loc] constructs a triand net type at location [loc]. *)

val net_w_or : Loc.t -> net
(** [net_w_or loc] constructs a wor net type at location [loc]. *)

val net_tri_or : Loc.t -> net
(** [net_tri_or loc] constructs a trior net type at location [loc]. *)

val net_tri_reg : Loc.t -> net
(** [net_tri_reg loc] constructs a trireg net type at location [loc]. *)

val net_u_wire : Loc.t -> net
(** [net_u_wire loc] constructs a uwire net type at location [loc]. *)

(** {3 Drive} *)

val drive_up : Loc.t -> drive
(** [drive_up loc] constructs a pull up drive at location [loc]. *)

val drive_down : Loc.t -> drive
(** [drive_down loc] constructs a pull down drive at location [loc]. *)

(** {3 Pragmas} *)

(** {4 Pragma Values} *)

val pragma_value_exprs : Loc.t -> pragma_expr list -> pragma_value
(** [pragma_value_exprs loc exprs] constructs a pragma expressions value at
    location [loc] with the value of [exprs]. *)

val pragma_value_num : Loc.t -> value -> pragma_value
(** [pragma_value_num loc value] constructs a pragma number value at location
    [loc] with the value [value]. *)

val pragma_value_string : Loc.t -> value -> pragma_value
(** [pragma_value_string loc value] constructs a pragma string value at location
    [loc] with the value [value]. *)

val pragma_value_ident : Loc.t -> name -> pragma_value
(** [pragma_value_ident loc name] constructs a pragma identifier value at
    location [loc] referencing the identier [name]. *)

(** {4 Pragma Expressions} *)

val pragma_expr : Loc.t -> name option -> pragma_value option -> pragma_expr
(** [pragma_expr_kwd loc kwd value] constrcuts a pragma expression at location
    [loc] named by the keyword [kwd] with the value [value]. *)

(** {3 Line Levels} *)

val level_entered : Loc.t -> level
(** [level_entered loc] constructs a entered level at location [loc]. *)

val level_exited : Loc.t -> level
(** [level_exited loc] constructs a exited level at location [loc]. *)

(** {3 Keywords} *)

val keywords_1364_1995 : Loc.t -> keywords
(** [keywords_1364_1995 loc] constructs the keyword set for IEEE 1364-1995 at
    location [loc]. *)

val keywords_1364_2001_noconfig : Loc.t -> keywords
(** [keywords_1364_2001_noconfig loc] constructs the keyword set for IEEE
    1364-2001-noconfig at location [loc]. *)

val keywords_1364_2001 : Loc.t -> keywords
(** [keywords_1364_2001 loc] constructs the keyword set for IEEE 1364-2001 at
    location [loc]. *)

val keywords_1364_2005 : Loc.t -> keywords
(** [keywords_1364_2005 loc] constructs the keyword set for IEEE 1364-2005 at
    location [loc]. *)

val keywords_1800_2005 : Loc.t -> keywords
(** [keywords_1800_2005 loc] constructs the keyword set for IEEE 1800-2005 at
    location [loc]. *)

val keywords_1800_2009 : Loc.t -> keywords
(** [keywords_1800_2009 loc] constructs the keyword set for IEEE 1800-2009 at
    location [loc]. *)

val keywords_1800_2012 : Loc.t -> keywords
(** [keywords_1800_2012 loc] constructs the keyword set for IEEE 1800-2012 at
    location [loc]. *)

(** {3 Directives} *)

val dir_reset_all : Loc.t -> dir
(** [dir_reset_all loc] constructs a reset all directive at location [loc]. *)

val dir_include : Loc.t -> incl -> dir
(** [dir_include loc src] constructs an include directive at location [loc]
    including source [src]. *)

val dir_define : Loc.t -> name -> params option -> body option -> dir
(** [dir_define loc name params body] constructs a define directive at location
    [loc] defining the macro named [name] with parameters [params].  If [body]
    is not [None], it is used as the text to expand. *)

val dir_undef : Loc.t -> name -> dir
(** [dir_undef loc name] constructs an undefine directive at location [loc]
    un-defining the macro named [name]. *)

val dir_undefine_all : Loc.t -> dir
(** [dir_undefine_all loc] constructs an undefine all directive at location
    [loc] which un-defines all current user-defined macros. *)

val dir_macro : Loc.t -> name -> args option -> dir
(** [dir_macro loc name args] constructs a macro expansion directive at location
    [loc] which expands the macro named [name] with the arguments [args]. *)

val dir_if_def : Loc.t -> name -> dir
(** [dir_if_def loc macro] constructs a primary case at location [loc] in the
    condition that [macro] is defined. *)

val dir_if_n_def : Loc.t -> name -> dir
(** [dir_if_n_def loc macro] constructs a primary case at location [loc] in the
    condition that [macro] is not defined. *)

val dir_els_if : Loc.t -> name -> dir
(** [dir_els_if loc macro] constructs a secondary case at location [loc] in the
    condition that [macro] is defined. *)

val dir_else : Loc.t -> dir
(** [dir_else loc] constructs an else directive at location [loc] marking the
    default case of a conditional. *)

val dir_end_if : Loc.t -> dir
(** [dir_end_if loc] constructs an end if directive at location [loc] that
    terminates a conditional. *)

val dir_timescale : Loc.t -> scale -> scale option -> dir
(** [dir_timescale loc time_unit time_prec] constructs a timescale directive at
    location [loc] with the time unit [time_unit] and the optional time
    precision [time_prec].  If [time_prec] is [None], [time_unit] is used as the
    time precision. *)

val dir_default_net_type : Loc.t -> name -> dir
(** [dir_default_net_type loc net_type] construcs a default net type directive
    at location [loc] that sets the default net type to [net_type]. *)

val dir_unconnected_drive : Loc.t -> name -> dir
(** [dir_unconnected_drive loc drive] constructs an unconnected drive directive
    at location [loc] that sets unconnected pins to drive as specified in
    [drive]. *)

val dir_no_unconnected_drive : Loc.t -> dir
(** [dir_no_unconnected_drive loc] constructs a no unconnected drive directive
    at location [loc] that resets the effect of a previous unconnected drive
    directive. *)

val dir_cell_define : Loc.t -> dir
(** [dir_cell_define loc] constructs a cell define directive at location [loc]
    that tags subsequent modules as cell modules. *)

val dir_end_cell_define : Loc.t -> dir
(** [dir_end_cell_define loc] constructs an end cell define directive at
    location [loc] that stops taging subsequent modules as cell modules. *)

val dir_pragma : Loc.t -> name -> pragma_expr list -> dir
(** [dir_pragma loc name exprs] constructs a pragma directive at location [loc]
    with the name [name] and the pragma expressions [exprs]. *)

val dir_line : Loc.t -> value -> value -> value -> dir
(** [dir_line number loc path level] constructs a line directive at location
    [loc] that overrides the current program location to be line [number] in
    [path].  If [level] is not [None], it determines whether an include was just
    entered or exited. *)

val dir_LINE : Loc.t -> dir
(** [dir_LINE loc] constructs a LINE directive at location [loc] that expands to
    the current line number. *)

val dir_FILE : Loc.t -> dir
(** [dir_FILE loc] constructs a FILE directive at location [loc] that expands to
    the name of the current file. *)

val dir_begin_keywords : Loc.t -> value -> dir
(** [dir_begin_keywords loc keywords] constructs a begin keywords directive at
    location [loc] that sets the set of keywords in use for the subsequent
    source. *)

val dir_end_keywords : Loc.t -> dir
(** [dir_end_keywords loc] constructs an end keywords directive at location
    [loc] that reverts the set of keywords to the default for the subsequent
    source. *)

(** {3 Segments} *)

val seg_source : Loc.t -> string -> seg
(** [seg_source loc src] constructs a segment of SystemVerilog source code at
    location [loc]. *)

val seg_directive : Loc.t -> dir -> seg
(** [seg_directive loc dir] constructs a directive segment at location [loc]. *)

(** {3 Files} *)

val file : Fpath.t -> seg list -> file
(** [file path segs] constructs a file located at [path] with the segments
    [segs]. *)

(** {3 Compilation Units} *)

val compilation_unit : file list -> t
(** [compilation_unit files] constructs a compilation unit of the files [files]. *)

(** {2 Pretty Printing} *)

val pp_name : name -> formatter -> unit
(** [pp_name name fmt] pretty-prints the name [name] to the formatter [fmt]. *)

val pp_value : value -> formatter -> unit
(** [pp_value value fmt] pretty-prints the value [value] to the formatter [fmt]. *)

val pp_param : param -> formatter -> unit
(** [pp_param param fmt] pretty-prints the macro parameter [param] to the
    formatter [fmt]. *)

val pp_params : params -> formatter -> unit
(** [pp_params params fmt] pretty-prints the macro parameter list [params] to
    the formatter [fmt]. *)

val pp_elem : elem -> formatter -> unit
(** [pp_elem elem fmt] pretty-prints the macro element [elem] to the formatter
    [fmt]. *)

val pp_line : line -> formatter -> unit
(** [pp_line line fmt] pretty-prints the macro line [line] to the formatter
    [fmt]. *)

val pp_body : body -> formatter -> unit
(** [pp_body body fmt] pretty-prints the macro body [body] to the formatter
    [fmt]. *)

val pp_args : args -> formatter -> unit
(** [pp_args args fmt] pretty-prints the macro arguments list [args] to the
    formatter [fmt]. *)

val pp_incl : incl -> formatter -> unit
(** [pp_incl incl fmt] pretty-prints the include source [incl] to the formatter
    [fmt]. *)

val pp_mag : mag -> formatter -> unit
(** [pp_mag mag fmt] pretty-prints the order of magnitude [mag] to the formatter
    [fmt]. *)

val pp_yoonit : yoonit -> formatter -> unit
(** [pp_yoonit yoonit fmt] pretty-prints the time unit [yoonit] to the formatter
    [fmt]. *)

val pp_scale : scale -> formatter -> unit
(** [pp_scale scale fmt] pretty-prints the scale [scale] to the formatter [fmt]. *)

val pp_net : net option -> formatter -> unit
(** [pp_net net fmt] pretty-prints the net type [net] to the formatter [fmt]. *)

val pp_drive : drive -> formatter -> unit
(** [pp_drive drive fmt] pretty-prints the drive direction [drive] to the
    formatter [fmt]. *)

val pp_pragma_value : pragma_value -> formatter -> unit
(** [pp_pragma_value value fmt] pretty-prints the pragma value [value] to the
    formatter [fmt]. *)

val pp_pragma_expr : pragma_expr -> formatter -> unit
(** [pp_pragma_expr expr fmt] pretty-prints the pragma expression [expr] to the
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
