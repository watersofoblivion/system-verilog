(* Names *)

open Format

open Common

type name =
  | Name of { loc: Loc.t; name: string }

let name loc name = Name { loc; name }

let pp_name = function
  | Name name -> dprintf "%s" name.name
