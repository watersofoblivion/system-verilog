(* Net Types *)

open Format

type net =
  | Wire
  | Tri
  | Tri0
  | Tri1
  | WAnd
  | TriAnd
  | WOr
  | TriOr
  | TriReg
  | UWire

let net_wire = Wire
let net_tri = Tri
let net_tri0 = Tri0
let net_tri1 = Tri1
let net_w_and = WAnd
let net_tri_and = TriAnd
let net_w_or = WOr
let net_tri_or = TriOr
let net_tri_reg = TriReg
let net_u_wire = UWire

let pp_net net =
  let net = match net with
    | Some Wire -> "wire"
    | Some Tri -> "tri"
    | Some Tri0 -> "tri0"
    | Some Tri1 -> "tri1"
    | Some WAnd -> "wand"
    | Some TriAnd -> "triand"
    | Some WOr -> "wor"
    | Some TriOr -> "trior"
    | Some TriReg -> "trireg"
    | Some UWire -> "uwire"
    | None -> "none"
  in
  dprintf "%s" net
