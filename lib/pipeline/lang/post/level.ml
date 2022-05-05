(* Line Levels *)

open Format

type level =
  | Entered
  | Exited

let level_entered = Entered
let level_exited = Exited

let pp_level level =
  let level = match level with
    | Some Entered -> 1
    | Some Exited -> 2
    | None -> 0
  in
  dprintf "%d" level
