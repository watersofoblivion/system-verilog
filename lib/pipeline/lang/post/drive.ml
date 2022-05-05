(* Unconnected Drive Directions *)

open Format

type drive =
  | PullUp
  | PullDown

let drive_up = PullUp
let drive_down = PullDown

let pp_drive drive =
  let dir = match drive with
    | PullUp -> 1
    | PullDown -> 0
  in
  dprintf "pull%d" dir
