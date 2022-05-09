(* Directives and Segments *)

open Format

open Common


type incl =
  | IncludePath of { loc: Loc.t; sys: bool; path: Value.value }
  | IncludeMacro of { loc: Loc.t; name: Name.name; args: args option }
and dir =
  | DirResetAll of { loc: Loc.t }
  | DirInclude of { loc: Loc.t; src: incl }
  | DirDefine of { loc: Loc.t; name: Name.name; params: Macro.params option; body: Macro.body option }
  | DirUndef of { loc: Loc.t; name: Name.name }
  | DirUndefineAll of { loc: Loc.t }
  | DirMacro of { loc: Loc.t; name: Name.name; args: args option }
  | DirIfDef of { loc: Loc.t; macro: Name.name }
  | DirIfNDef of { loc: Loc.t; macro: Name.name }
  | DirElsIf of { loc: Loc.t; macro: Name.name }
  | DirElse of { loc: Loc.t }
  | DirEndIf of { loc: Loc.t }
  | DirTimescale of { loc: Loc.t; yoonit: Timescale.scale; prec: Timescale.scale option }
  | DirDefaultNetType of { loc: Loc.t; net: Net.net option }
  | DirUnconnectedDrive of { loc: Loc.t; drive: Drive.drive }
  | DirNoUnconnectedDrive of { loc: Loc.t }
  | DirCellDefine of { loc: Loc.t }
  | DirEndCellDefine of { loc: Loc.t }
  | DirPragma of { loc: Loc.t; exprs: Pragma.pragma_expr list }
  | DirLine of { loc: Loc.t; number: int; path: Value.value; level: Level.level option }
  | DirFILE of { loc: Loc.t }
  | DirLINE of { loc: Loc.t }
  | DirBeginKeywords of { loc: Loc.t; keywords: Keywords.keywords }
  | DirEndKeywords of { loc: Loc.t }
and args =
  | Args of { loc: Loc.t; args: seg list }
and seg =
  | SegSource of { loc: Loc.t; src: string }
  | SegDirective of { loc: Loc.t; dir: dir }

let incl_path loc sys path = IncludePath { loc; sys; path }
let incl_macro loc name args = IncludeMacro { loc; name; args }

let dir_reset_all loc = DirResetAll { loc }
let dir_include loc src = DirInclude { loc; src }
let dir_define loc name params body = DirDefine { loc; name; params; body }
let dir_undef loc name = DirUndef { loc; name }
let dir_undefine_all loc = DirUndefineAll { loc }
let dir_macro loc name args = DirMacro { loc; name; args }
let dir_if_def loc macro = DirIfDef { loc; macro }
let dir_if_n_def loc macro = DirIfNDef { loc; macro }
let dir_els_if loc macro = DirElsIf { loc; macro }
let dir_else loc = DirElse { loc }
let dir_end_if loc = DirEndIf { loc }
let dir_timescale loc yoonit prec = DirTimescale { loc; yoonit; prec }
let dir_default_net_type loc net = DirDefaultNetType { loc; net }
let dir_unconnected_drive loc drive = DirUnconnectedDrive { loc; drive }
let dir_no_unconnected_drive loc = DirNoUnconnectedDrive { loc }
let dir_cell_define loc = DirCellDefine { loc }
let dir_end_cell_define loc = DirEndCellDefine { loc }
let dir_pragma loc exprs = DirPragma { loc; exprs }
let dir_line loc number path level = DirLine { loc; number; path; level }
let dir_FILE loc = DirFILE { loc }
let dir_LINE loc = DirLINE { loc }
let dir_begin_keywords loc keywords = DirBeginKeywords { loc; keywords }
let dir_end_keywords loc = DirEndKeywords { loc }

let args loc args = Args { loc; args }

let seg_source loc src = SegSource { loc; src }
let seg_directive loc dir = SegDirective { loc; dir }

let rec pp_incl = function
  | IncludePath incl ->
    if incl.sys
    then dprintf "<%t>" (Value.pp_value incl.path)
    else dprintf "\"%t\"" (Value.pp_value incl.path)
  | IncludeMacro incl ->
    let pp fmt args = fprintf fmt "(%t)" (pp_args args) in
    dprintf "`%t%a"
      (Name.pp_name incl.name)
      (pp_print_option pp) incl.args

and pp_dir = function
  | DirResetAll _ -> dprintf "`resetall"
  | DirInclude dir -> dprintf "`include %t" (pp_incl dir.src)
  | DirDefine dir ->
    let pp_params fmt params = fprintf fmt "(%t)" (Macro.pp_params params) in
    let pp_body fmt body = fprintf fmt " %t" (Macro.pp_body body) in
    dprintf "`define %t%a%a"
      (Name.pp_name dir.name)
      (pp_print_option pp_params) dir.params
      (pp_print_option pp_body) dir.body
  | DirUndef dir -> dprintf "`undef %t" (Name.pp_name dir.name)
  | DirUndefineAll _ -> dprintf "`undefineall"
  | DirMacro dir ->
    let pp fmt args = fprintf fmt "(%t)" (pp_args args) in
    dprintf "`%t%a"
      (Name.pp_name dir.name)
      (pp_print_option pp) dir.args
  | DirIfDef dir -> dprintf "`ifdef %t" (Name.pp_name dir.macro)
  | DirIfNDef dir -> dprintf "`ifndef %t" (Name.pp_name dir.macro)
  | DirElsIf dir -> dprintf "`elsif %t" (Name.pp_name dir.macro)
  | DirElse _ -> dprintf "`else"
  | DirEndIf _ -> dprintf "`endif"
  | DirTimescale dir ->
    let pp_prec fmt prec = dprintf "/%t" (Timescale.pp_scale prec) fmt in
    dprintf "`timescale %t%a" (Timescale.pp_scale dir.yoonit) (pp_print_option pp_prec) dir.prec
  | DirDefaultNetType dir -> dprintf "`default_nettype %t" (Net.pp_net dir.net)
  | DirUnconnectedDrive dir -> dprintf "`unconnected_drive %t" (Drive.pp_drive dir.drive)
  | DirNoUnconnectedDrive _ -> dprintf "`no_unconnected_drive"
  | DirCellDefine _ -> dprintf "`celldefine"
  | DirEndCellDefine _ -> dprintf "`endcelldefine"
  | DirPragma dir ->
    let pp fmt v = Pragma.pp_pragma_expr v fmt in
    let pp_sep fmt _ = fprintf fmt ", " in
    dprintf "`pragma %a"
      (pp_print_list ~pp_sep pp) dir.exprs
  | DirLine dir -> dprintf "`line %d \"%t\" %t" dir.number (Value.pp_value dir.path) (Level.pp_level dir.level)
  | DirFILE _ -> dprintf "`__FILE__"
  | DirLINE _ -> dprintf "`__LINE__"
  | DirBeginKeywords dir -> dprintf "`begin_keywords %t" (Keywords.pp_keywords dir.keywords)
  | DirEndKeywords _ -> dprintf "`end_keywords"

and pp_args args =
  let pp fmt arg = pp_seg arg fmt in
  match args with
    | Args args ->
      let pp_sep fmt _ = fprintf fmt ", " in
      dprintf "%a"
        (pp_print_list ~pp_sep pp) args.args

and pp_seg = function
  | SegSource seg -> dprintf "%s" seg.src
  | SegDirective seg -> dprintf "%t" (pp_dir seg.dir)