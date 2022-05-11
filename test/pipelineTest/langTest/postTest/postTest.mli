(** {1 Post-processed Compilation Unit Tests} *)

open Common

open OUnit2

(** {2 Test Suite} *)

val suite : test
(** [suite] is the unit test suite. *)

(** {2 Fixtures} *)

(** {3 Names} *)

val name : ?loc:Loc.t -> ?name:string -> unit -> Post.name
(** [name ?loc ?name ()] constructs a name at location [loc] named [name]. *)

(** {3 Values} *)

val value : ?loc:Loc.t -> ?value:string -> unit -> Post.value
(** [name ?loc ?value ()] constructs a value at location [loc] with value
    [value]. *)

(** {3 Macros} *)

(** {4 Parameters} *)

val param : ?loc:Loc.t -> ?name:Post.name -> ?default:Post.value option -> unit -> Post.param
(** [name ?loc ?name ?value ()] constructs a macro parameter at location [loc]
    named [name] with the default value [default]. *)

val params : ?loc:Loc.t -> ?params:Post.param list -> unit -> Post.params
(** [params ?loc ?params ()] constructs a macro parameter list at location [loc]
    composed of the parameters [params]. *)

(** {4 Bodies} *)

val elem_source : ?loc:Loc.t -> ?value:Post.value -> unit -> Post.elem
(** [elem_source ?loc ?value ()] constructs a source code macro body element at
    location [loc] with the source [value]. *)

val elem_var : ?loc:Loc.t -> ?name:Post.name -> unit -> Post.elem
(** [elem_var ?loc ?name ()] constructs a variable reference macro body element
    at location [loc] referencing the parameter named [name]. *)

val line : ?loc:Loc.t -> ?elems:Post.elem list -> unit -> Post.line
(** [line ?loc ?elems ()] constructs a macro body line at location [loc]
    composed of the elements [elems]. *)

val body : ?loc:Loc.t -> ?lines:Post.line list -> unit -> Post.body
(** [body ?loc ?lines ()] constructs a macro body at location [loc] composed of
    the lines [lines]. *)

(** {4 Arguments} *)

val args : ?loc:Loc.t -> ?args:Post.value list -> unit -> Post.args
(** [args loc args] constructs a macro arguments list at location [loc] composed
    of the arguments [args]. *)

(** {3 Includes} *)

val incl_path : ?loc:Loc.t -> ?sys:bool -> ?path:Post.value -> unit -> Post.incl
(** [incl_path ?loc ?sys ?path ()] constructs a path include source at location
    [loc] including the file at the path [path].  If [sys] is [true], only the
    system paths are searched. (I.e., the [<...>] syntax was used.) *)

val incl_macro : ?loc:Loc.t -> ?name:Post.name -> ?args:Post.args option -> unit -> Post.incl
(** [incl_macro ?loc ?name ?args ()] constructs a macro include source at
    location [loc] that includes the results of expanding the macro named [name]
    with the arguments [args]. *)

(** {3 Timescales} *)

(** {4 Orders of Magnitude} *)

val mag_1 : ?loc:Loc.t -> unit -> Post.mag
(** [mag_1 ?loc ()] constructs a zero orders of magnitude at location [loc]. *)

val mag_10 : ?loc:Loc.t -> unit -> Post.mag
(** [mag_10 ?loc ()] constructs a one order of magnitude at location [loc]. *)

val mag_100 : ?loc:Loc.t -> unit -> Post.mag
(** [mag_100 ?loc ()] constructs a two orders of magnitude at location [loc]. *)

(** {4 Time Units} *)

val unit_s : ?loc:Loc.t -> unit -> Post.yoonit
(** [unit_s ?loc ()] constructs a seconds unit at location [loc]. *)

val unit_ms : ?loc:Loc.t -> unit -> Post.yoonit
(** [unit_ms ?loc ()] constructs a millisecond unit at location [loc]. *)

val unit_us : ?loc:Loc.t -> unit -> Post.yoonit
(** [unit_us ?loc ()] constructs a microsecond unit at location [loc]. *)

val unit_ns : ?loc:Loc.t -> unit -> Post.yoonit
(** [unit_ns ?loc ()] constructs a nanosecond unit at location [loc]. *)

val unit_ps : ?loc:Loc.t -> unit -> Post.yoonit
(** [unit_ps ?loc ()] constructs a picosecond unit at location [loc]. *)

val unit_fs : ?loc:Loc.t -> unit -> Post.yoonit
(** [unit_fs ?loc ()] constructs a femtosecond unit at location [loc]. *)

(** {4 Scales} *)

val scale : ?loc:Loc.t -> ?mag:Post.value -> ?yoonit:Post.value -> unit -> Post.scale
(** [scale ?loc ?mag ?yoonit ()] constructs a scale of [mag] [yoonit]s at
    location [loc]. *)

(** {3 Net Types} *)

val net_wire : ?loc:Loc.t -> unit -> Post.net
(** [net_wire ?loc ()] constructs a wire net type at location [loc]. *)

val net_tri : ?loc:Loc.t -> unit -> Post.net
(** [net_tri ?loc ()] constructs a tri net type at location [loc]. *)

val net_tri0 : ?loc:Loc.t -> unit -> Post.net
(** [net_tri0 ?loc ()] constructs a tri0 net type at location [loc]. *)

val net_tri1 : ?loc:Loc.t -> unit -> Post.net
(** [net_tri1 ?loc ()] constructs a tri1 net type at location [loc]. *)

val net_w_and : ?loc:Loc.t -> unit -> Post.net
(** [net_w_and ?loc ()] constructs a wand net type at location [loc]. *)

val net_tri_and : ?loc:Loc.t -> unit -> Post.net
(** [net_tri_and ?loc ()] constructs a triand net type at location [loc]. *)

val net_w_or : ?loc:Loc.t -> unit -> Post.net
(** [net_w_or ?loc ()] constructs a wor net type at location [loc]. *)

val net_tri_or : ?loc:Loc.t -> unit -> Post.net
(** [net_tri_or ?loc ()] constructs a trior net type at location [loc]. *)

val net_tri_reg : ?loc:Loc.t -> unit -> Post.net
(** [net_tri_reg ?loc ()] constructs a trireg net type at location [loc]. *)

val net_u_wire : ?loc:Loc.t -> unit -> Post.net
(** [net_u_wire ?loc ()] constructs a uwire net type at location [loc]. *)

(** {3 Drives} *)

val drive_up : ?loc:Loc.t -> unit -> Post.drive
(** [drive_up ?loc ()] constructs a pull up drive at location [loc]. *)

val drive_down : ?loc:Loc.t -> unit -> Post.drive
(** [drive_down ?loc ()] constructs a pull down drive at location [loc]. *)

(** {3 Pragmas} *)

(** {4 Pragma Values} *)

val pragma_value_exprs : ?loc:Loc.t -> ?exprs:Post.pragma_expr list -> unit -> Post.pragma_value
(** [pragma_value_exprs ?loc ?exprs ()] constructs a pragma expressions value at
    location [loc] with the value of [exprs]. *)

val pragma_value_num : ?loc:Loc.t -> ?value:Post.value -> unit -> Post.pragma_value
(** [pragma_value_num ?loc ?value ()] constructs a pragma number value at
    location [loc] with the value [value]. *)

val pragma_value_string : ?loc:Loc.t -> ?value:Post.value -> unit -> Post.pragma_value
(** [pragma_value_string ?loc ?value ()] constructs a pragma string value at
    location [loc] with the value [value]. *)

val pragma_value_ident : ?loc:Loc.t -> ?name:Post.name -> unit -> Post.pragma_value
(** [pragma_value_ident ?loc ?name ()] constructs a pragma identifier value at
    location [loc] referencing the identier [name]. *)

(** {4 Pragma Expressions} *)

val pragma_expr : ?loc:Loc.t -> ?kwd:Post.name option -> ?value:Post.pragma_value option -> unit -> Post.pragma_expr
(** [pragma_expr ?loc ?value ()] constrcuts an unnamed expression at location
    [loc] with the keyword [kwd] and value [value]. *)

(** {3 Line Levels} *)

val level_entered : ?loc:Loc.t -> unit -> Post.level
(** [level_entered ?loc ()] constructs an entered line level at location [loc]. *)

val level_exited : ?loc:Loc.t -> unit -> Post.level
(** [level_exited ?loc ()] constructs an exited line level at location [loc]. *)

(** {3 Keywords} *)

val keywords_1364_1995 : ?loc:Loc.t -> unit -> Post.keywords
(** [keywords_1364_1995 ?loc ()] constructs the IEEE 1364-1995 keywords at
    location [loc]. *)

val keywords_1364_2001_noconfig : ?loc:Loc.t -> unit -> Post.keywords
(** [keywords_1364_2001_noconfig ?loc ()] constructs the IEEE 1364-2001-noconfig
    keywords at location [loc]. *)

val keywords_1364_2001 : ?loc:Loc.t -> unit -> Post.keywords
(** [keywords_1364_2001 ?loc ()] constructs the IEEE 1364-2001 keywords at
    location [loc]. *)

val keywords_1364_2005 : ?loc:Loc.t -> unit -> Post.keywords
(** [keywords_1364_2005 ?loc ()] constructs the IEEE 1364-2005 keywords at
    location [loc]. *)

val keywords_1800_2005 : ?loc:Loc.t -> unit -> Post.keywords
(** [keywords_1800_2005 ?loc ()] constructs the IEEE 1800-2005 keywords at
    location [loc]. *)

val keywords_1800_2009 : ?loc:Loc.t -> unit -> Post.keywords
(** [keywords_1800_2009 ?loc ()] constructs the IEEE 1800-2009 keywords at
    location [loc]. *)

val keywords_1800_2012 : ?loc:Loc.t -> unit -> Post.keywords
(** [keywords_1800_2012 ?loc ()] constructs the IEEE 1800-2012 keywords at
    location [loc]. *)

(** {3 Directives} *)

val dir_reset_all : ?loc:Loc.t -> unit -> Post.dir
(** [dir_reset_all ?loc ()] constructs a reset all directive at location [loc]. *)

val dir_include : ?loc:Loc.t -> ?src:Post.incl -> unit -> Post.dir
(** [dir_include ?loc ?src ()] constructs an include directive at location [loc]
    including the file specified by [src]. *)

val dir_define : ?loc:Loc.t -> ?name:Post.name -> ?params:Post.params option -> ?body:Post.body option -> unit -> Post.dir
(** [dir_define ?loc ?name ?params ?body ()] constructs a define directive at
    location [loc] defining the macro name [name], with parameters [params] and
    the body [body]. *)

val dir_undef : ?loc:Loc.t -> ?name:Post.name -> unit -> Post.dir
(** [dir_undef ?loc ?name ()] constructs an undefine directive at location [loc]
    undefining the macro named [name]. *)

val dir_undefine_all : ?loc:Loc.t -> unit -> Post.dir
(** [dir_undefine_all ?loc ()] constructs an undefine all directive at location
    [loc]. *)

val dir_macro : ?loc:Loc.t -> ?name:Post.name -> ?args:Post.args option -> unit -> Post.dir
(** [dir_macro ?loc ?name ?args ()] constructs a macro expansion directive at
    location [loc] applying the macro [name] to the arguments [args]. *)

val dir_if_def : ?loc:Loc.t -> ?macro:Post.name -> unit -> Post.dir
(** [dir_if_def ?loc ?macro ()] construcs a conditional directive at location
    [loc] testing if the macro [macro] is defined. *)

val dir_if_n_def : ?loc:Loc.t -> ?macro:Post.name -> unit -> Post.dir
(** [dir_if_n_def ?loc ?macro ()] construcs a negative conditional directive at
    location [loc] testing if the macro [macro] is not defined. *)

val dir_els_if : ?loc:Loc.t -> ?macro:Post.name -> unit -> Post.dir
(** [dir_els_if ?loc ?macro ()] construcs a secondary conditional directive at
    location [loc] testing if the macro [macro] is defined. *)

val dir_else : ?loc:Loc.t -> unit -> Post.dir
(** [dir_else ?loc ()] constructs a default conditional directive at location
    [loc]. *)

val dir_end_if : ?loc:Loc.t -> unit -> Post.dir
(** [dir_end_if ?loc ()] constructs a conditional termination directive at
    location [loc]. *)

val dir_timescale : ?loc:Loc.t -> ?yoonit:Post.scale -> ?prec:Post.scale option -> unit -> Post.dir
(** [dir_timescale ?loc ?mag ?yoonit ()] constructs a timescale directive at
    location [loc] with time unit [yoonit] and precision [prec]. *)

val dir_default_net_type : ?loc:Loc.t -> ?net:Post.name -> unit -> Post.dir
(** [dir_default_net_type ?loc ?net ()] constructs a default net type directive
    at location [loc] with the net type [net]. *)

val dir_unconnected_drive : ?loc:Loc.t -> ?drive:Post.name -> unit -> Post.dir
(** [dir_unconnected_drive ?loc ?drive ()] constructs a drive unconnected pins
    directive at location [loc] with the drive direction [drive] .*)

val dir_no_unconnected_drive : ?loc:Loc.t -> unit -> Post.dir
(** [dir_no_unconnected_drive ?loc ()] constructs a stop driving unconnected
    pins directive at location [loc]. *)

val dir_cell_define : ?loc:Loc.t -> unit -> Post.dir
(** [dir_cell_define ?loc ()] constructs a cell module directive at location
    [loc]. *)

val dir_end_cell_define : ?loc:Loc.t -> unit -> Post.dir
(** [dir_no_cell_define ?loc ()] constructs an end cell module directive at
    location [loc]. *)

val dir_pragma : ?loc:Loc.t -> ?name:Post.name -> ?exprs:Post.pragma_expr list -> unit -> Post.dir
(** [dir_pragma ?loc ?name ?exprs ()] constructs a pragma directive a location
    [loc] with the name [name] and the expressions [exprs]. *)

val dir_line : ?loc:Loc.t -> ?number:Post.value -> ?path:Post.value -> ?level:Post.value -> unit -> Post.dir
(** [dir_line ?loc ?number ?path ?level ()] constructs a line level directive at
    location [loc] with line [number] in the file [path] with level [level]. *)

val dir_FILE : ?loc:Loc.t -> unit -> Post.dir
(** [dir_FILE ?loc ()] constructs a FILE directive at location [loc]. *)

val dir_LINE : ?loc:Loc.t -> unit -> Post.dir
(** [dir_LINE ?loc ()] constructs a LINE directive at location [loc]. *)

val dir_begin_keywords : ?loc:Loc.t -> ?keywords:Post.value -> unit -> Post.dir
(** [dir_begin_keywords ?loc ?keywords ()] constructs a begin keywords directive
    at location [loc] with the keywords [keywords]. *)

val dir_end_keywords : ?loc:Loc.t -> unit -> Post.dir
(** [dir_end_keywords ?loc ()] constructs an end keywords directive at location
    [loc]. *)

(** {3 Segments} *)

val seg_source : ?loc:Loc.t -> ?src:string -> unit -> Post.seg
(** [seg_source ?loc ?src ()] constructs a source segment at location [loc] with
    the source code [src]. *)

val seg_directive : ?loc:Loc.t -> ?dir:Post.dir -> unit -> Post.seg
(** [seg_directive ?loc ?dir ()] constructs a directive segment at location
    [loc] with the directive [dir]. *)

(** {2 Assertions} *)

val assert_name_equal : ctxt:test_ctxt -> Post.name -> Post.name -> unit
(** [assert_name_equal ~ctxt expected actual] asserts that the name [actual] is
    equal to the name [expected]. *)

val assert_value_equal : ctxt:test_ctxt -> Post.value -> Post.value -> unit
(** [assert_value_equal ~ctxt expected actual] asserts that the value [actual]
    is equal to the value [expected]. *)

val assert_param_equal : ctxt:test_ctxt -> Post.param -> Post.param -> unit
(** [assert_param_equal ~ctxt expected actual] asserts that the parameter
    [actual] is equal to the parameter [expected]. *)

val assert_params_equal : ctxt:test_ctxt -> Post.params -> Post.params -> unit
(** [assert_params_equal ~ctxt expected actual] asserts that the parameter list
    [actual] are equal to the parameter list [expected]. *)

val assert_elem_equal : ctxt:test_ctxt -> Post.elem -> Post.elem -> unit
(** [assert_elem_equal ~ctxt expected actual] asserts that the macro body
    element [actual] is equal to the macro body element [expected]. *)

val assert_line_equal : ctxt:test_ctxt -> Post.line -> Post.line -> unit
(** [assert_line_equal ~ctxt expected actual] asserts that the macro body line
    [actual] is equal to the macro body line [expected]. *)

val assert_body_equal : ctxt:test_ctxt -> Post.body -> Post.body -> unit
(** [assert_body_equal ~ctxt expected actual] asserts that the macro body
    [actual] is equal to the macro body [expected]. *)

val assert_args_equal : ctxt:test_ctxt -> Post.args -> Post.args -> unit
(** [assert_args_equal ~ctxt expected actual] asserts that the macro arguments
    [actual] are equal to the macro arguments [expected]. *)

val assert_incl_equal : ctxt:test_ctxt -> Post.incl -> Post.incl -> unit
(** [assert_incl_equal ~ctxt expected actual] asserts that the include value
    [actual] is equal to the include value [expected]. *)

val assert_mag_equal : ctxt:test_ctxt -> Post.mag -> Post.mag -> unit
(** [assert_mag_equal ~ctxt expected actual] asserts that the order of magnitude
    [actual] is equal to the order of magnitude [expected]. *)

val assert_yoonit_equal : ctxt:test_ctxt -> Post.yoonit -> Post.yoonit -> unit
(** [assert_yoonit_equal ~ctxt expected actual] asserts that the time unit
    [actual] is equal to the time unit [expected]. *)

val assert_scale_equal : ctxt:test_ctxt -> Post.scale -> Post.scale -> unit
(** [assert_scale_equal ~ctxt expected actual] asserts that the scale [actual]
    is equal to the scale [expected]. *)

val assert_net_equal : ctxt:test_ctxt -> Post.net -> Post.net -> unit
(** [assert_net_equal ~ctxt expected actual] asserts that the net type [actual]
    is equal to the net type [expected]. *)

val assert_drive_equal : ctxt:test_ctxt -> Post.drive -> Post.drive -> unit
(** [assert_drive_equal ~ctxt expected actual] asserts that the drive type
    [actual] is equal to the drive type [expected]. *)

val assert_pragma_value_equal : ctxt:test_ctxt -> Post.pragma_value -> Post.pragma_value -> unit
(** [assert_pragma_value_equal ~ctxt expected actual] asserts that the pragma
    value [actual] is equal to the pragma value [expected]. *)

val assert_pragma_expr_equal : ctxt:test_ctxt -> Post.pragma_expr -> Post.pragma_expr -> unit
(** [assert_pragma_expr_equal ~ctxt expected actual] asserts that the pragma
    expression [actual] is equal to the pragma expression [expected]. *)

val assert_level_equal : ctxt:test_ctxt -> Post.level -> Post.level -> unit
(** [assert_level_equal ~ctxt expected actual] asserts that the line level
    [actual] is equal to the line level [expected]. *)

val assert_keywords_equal : ctxt:test_ctxt -> Post.keywords -> Post.keywords -> unit
(** [assert_keywords_equal ~ctxt expected actual] asserts that the set of
    keywords [actual] is equal to the set of keywords [expected]. *)

val assert_dir_equal : ctxt:test_ctxt -> Post.dir -> Post.dir -> unit
(** [assert_dir_equal ~ctxt expected actual] asserts that the compiler directive
    [actual] is equal to the compiler directive [expected]. *)

val assert_seg_equal : ctxt:test_ctxt -> Post.seg -> Post.seg -> unit
(** [assert_seg_equal ~ctxt expected actual] asserts that the segment [actual]
    is equal to the segment [expected]. *)
