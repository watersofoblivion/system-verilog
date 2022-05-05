(* Directives and Segments *)

open Format

open Common

type dir =
  | ResetAll
  | Include of { sys: bool; path: Fpath.t }
  | Define of { name: string; params: Param.param list; body: string option }
  | Undef of { name: string }
  | UndefineAll
  | Macro of { name: string; args: seg list }
  | IfDef of { macro: string }
  | IfNDef of { macro: string }
  | ElsIf of { macro: string }
  | Else
  | EndIf
  | Timescale of { time_unit: Timescale.s; time_prec: Timescale.s option }
  | DefaultNetType of { net: Net.net option }
  | UnconnectedDrive of { drive: Drive.drive }
  | NoUnconnectedDrive
  | CellDefine
  | EndCellDefine
  | Pragma of { name: string; exprs: (string option * string option) list }
  | Line of { number: int; path: Fpath.t; level: Level.level option }
  | FILE
  | LINE
  | BeginKeywords of { keywords: Keywords.keywords }
  | EndKeywords
and seg =
  | Source of { src: string }
  | Directive of { dir: dir }

let dir_reset_all = ResetAll
let dir_include sys path = Include { sys; path }
let dir_define name params body = Define { name; params; body }
let dir_undef name = Undef { name }
let dir_undefine_all = UndefineAll
let dir_macro name args = Macro { name; args }
let dir_if_def macro = IfDef { macro }
let dir_if_n_def macro = IfNDef { macro }
let dir_els_if macro = ElsIf { macro }
let dir_else = Else
let dir_end_if = EndIf
let dir_timescale time_unit time_prec = Timescale { time_unit; time_prec }
let dir_default_net_type net = DefaultNetType { net }
let dir_unconnected_drive drive = UnconnectedDrive { drive }
let dir_no_unconnected_drive = NoUnconnectedDrive
let dir_cell_define = CellDefine
let dir_end_cell_define = EndCellDefine
let dir_pragma _ _ = failwith "TODO"
let dir_line number path level = Line { number; path; level }
let dir_FILE = FILE
let dir_LINE = LINE
let dir_begin_keywords keywords = BeginKeywords { keywords }
let dir_end_keywords = EndKeywords

let seg_source src = Source { src }
let seg_directive dir = Directive { dir }

let rec pp_dir = function
  | ResetAll -> dprintf "`resetall"
  | Include dir ->
    let path = Fpath.to_string dir.path in
    if dir.sys
    then dprintf "`include <%s>" path
    else dprintf "`include %S" path
  | Define _ -> failwith "TODO"
  | Undef dir -> dprintf "`undef %s" dir.name
  | UndefineAll -> dprintf "`undefineall"
  | Macro _ -> failwith "TODO"
  | IfDef dir -> dprintf "`ifdef %s" dir.macro
  | IfNDef dir -> dprintf "`ifndef %s" dir.macro
  | ElsIf dir -> dprintf "`elsif %s" dir.macro
  | Else -> dprintf "`else"
  | EndIf -> dprintf "`endif"
  | Timescale dir ->
    let pp_prec fmt prec = dprintf "/%t" (Timescale.pp_s prec) fmt in
    dprintf "`timescale %t%a" (Timescale.pp_s dir.time_unit) (pp_print_option pp_prec) dir.time_prec
  | DefaultNetType dir -> dprintf "`default_nettype %t" (Net.pp_net dir.net)
  | UnconnectedDrive dir -> dprintf "`unconnected_drive %t" (Drive.pp_drive dir.drive)
  | NoUnconnectedDrive -> dprintf "`no_unconnected_drive"
  | CellDefine -> dprintf "`celldefine"
  | EndCellDefine -> dprintf "`endcelldefine"
  | Pragma _ -> failwith "TODO"
  | Line dir -> dprintf "`line %d %S %t" dir.number (Fpath.to_string dir.path) (Level.pp_level dir.level)
  | FILE -> dprintf "`__FILE__"
  | LINE -> dprintf "`__LINE__"
  | BeginKeywords dir -> dprintf "`begin_keywords %t" (Keywords.pp_keywords dir.keywords)
  | EndKeywords -> dprintf "`end_keywords"
and pp_seg = function
  | Source seg -> dprintf "%s" seg.src
  | Directive seg -> dprintf "%t" (pp_dir seg.dir)
