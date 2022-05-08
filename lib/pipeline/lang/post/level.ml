(* Line Levels *)

open Format

open Common

type level =
  | LevelEntered of { loc: Loc.t }
  | LevelExited of { loc: Loc.t }

let level_entered loc = LevelEntered { loc }
let level_exited loc = LevelExited { loc }

let pp_level level =
  let level = match level with
    | Some (LevelEntered _) -> 1
    | Some (LevelExited _) -> 2
    | None -> 0
  in
  dprintf "%d" level
