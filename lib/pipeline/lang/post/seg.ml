(* Segments *)

open Format

open Common

type seg =
  | SegSource of { loc: Loc.t; src: string }
  | SegDirective of { loc: Loc.t; dir: Dir.dir }

let seg_source loc src = SegSource { loc; src }
let seg_directive loc dir = SegDirective { loc; dir }

let pp_seg = function
  | SegSource seg -> dprintf "%s" seg.src
  | SegDirective seg -> dprintf "%t" (Dir.pp_dir seg.dir)
