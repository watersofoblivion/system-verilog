(* Unconnected Drive Directions *)

open Format

open Common

type drive =
  | DriveUp of { loc: Loc.t }
  | DriveDown of { loc: Loc.t }

let drive_up loc = DriveUp { loc }
let drive_down loc = DriveDown { loc }

let pp_drive drive =
  let dir = match drive with
    | DriveUp _ -> 1
    | DriveDown _ -> 0
  in
  dprintf "pull%d" dir
