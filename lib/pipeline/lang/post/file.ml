(* Files *)

open Format

type file =
  | File of { path: Fpath.t; segs: DirArgSeg.seg list }

let file path segs = File { path; segs }

let pp_file = function
  | File file ->
    let pp_sep fmt _ = dprintf "@ @ " fmt in
    let pp_seg fmt seg = DirArgSeg.pp_seg seg fmt in
    dprintf "@[<v>%a@]" (pp_print_list ~pp_sep pp_seg) file.segs
