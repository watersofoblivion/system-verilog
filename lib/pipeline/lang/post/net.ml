(* Net Types *)

open Format

open Common

type net =
  | NetWire of { loc: Loc.t }
  | NetTri of { loc: Loc.t }
  | NetTri0 of { loc: Loc.t }
  | NetTri1 of { loc: Loc.t }
  | NetWAnd of { loc: Loc.t }
  | NetTriAnd of { loc: Loc.t }
  | NetWOr of { loc: Loc.t }
  | NetTriOr of { loc: Loc.t }
  | NetTriReg of { loc: Loc.t }
  | NetUWire of { loc: Loc.t }

let net_wire loc = NetWire { loc }
let net_tri loc = NetTri { loc }
let net_tri0 loc = NetTri0 { loc }
let net_tri1 loc = NetTri1 { loc }
let net_w_and loc = NetWAnd { loc }
let net_tri_and loc = NetTriAnd { loc }
let net_w_or loc = NetWOr { loc }
let net_tri_or loc = NetTriOr { loc }
let net_tri_reg loc = NetTriReg { loc }
let net_u_wire loc = NetUWire { loc }

let pp_net net =
  let net = match net with
    | Some (NetWire _) -> "wire"
    | Some (NetTri _) -> "tri"
    | Some (NetTri0 _) -> "tri0"
    | Some (NetTri1 _) -> "tri1"
    | Some (NetWAnd _) -> "wand"
    | Some (NetTriAnd _) -> "triand"
    | Some (NetWOr _) -> "wor"
    | Some (NetTriOr _) -> "trior"
    | Some (NetTriReg _) -> "trireg"
    | Some (NetUWire _) -> "uwire"
    | None -> "none"
  in
  dprintf "%s" net
