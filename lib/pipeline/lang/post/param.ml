(* Macro Parameters *)

open Format

open Common

type param =
  | Param of { loc: Loc.t; name: Name.name; default: Value.value option }

let param loc name default = Param { loc; name; default }

let pp_param param =
  let pp_default fmt default = fprintf fmt " = %t" (Value.pp_value default) in
  match param with
    | Param param ->
        dprintf "%t%a"
          (Name.pp_name param.name)
          (pp_print_option pp_default) param.default
