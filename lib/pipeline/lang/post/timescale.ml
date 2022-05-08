(* Timescales *)

open Format

open Common

type mag =
  | Magnitude1 of { loc: Loc.t }
  | Magnitude10 of { loc: Loc.t }
  | Magnitude100 of { loc: Loc.t }

type yoonit =
  | UnitS of { loc: Loc.t }
  | UnitMs of { loc: Loc.t }
  | UnitUs of { loc: Loc.t }
  | UnitNs of { loc: Loc.t }
  | UnitPs of { loc: Loc.t }
  | UnitFs of { loc: Loc.t }

type scale =
  | Scale of { loc: Loc.t; mag: mag; yoonit: yoonit }

let mag_1 loc = Magnitude1 { loc }
let mag_10 loc = Magnitude10 { loc }
let mag_100 loc = Magnitude100 { loc }

let unit_s loc = UnitS { loc }
let unit_ms loc = UnitMs { loc }
let unit_us loc = UnitUs { loc }
let unit_ns loc = UnitNs { loc }
let unit_ps loc = UnitPs { loc }
let unit_fs loc = UnitFs { loc }

let scale loc mag yoonit = Scale { loc; mag; yoonit }

let pp_mag = function
  | Magnitude1 _ -> dprintf "1"
  | Magnitude10 _ -> dprintf "10"
  | Magnitude100 _ -> dprintf "100"

let pp_yoonit = function
  | UnitS _ -> dprintf "s"
  | UnitMs _ -> dprintf "ms"
  | UnitUs _ -> dprintf "us"
  | UnitNs _ -> dprintf "ns"
  | UnitPs _ -> dprintf "ps"
  | UnitFs _ -> dprintf "fs"

let pp_scale = function
  | Scale scale ->
    dprintf "%t%t" (pp_mag scale.mag) (pp_yoonit scale.yoonit)
