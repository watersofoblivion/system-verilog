(* Macro Parameters *)

open Format

type param =
  | Param of { name: string; default: string option }

let param name default = Param { name; default }

let pp_param param =
  let pp_default fmt default = fprintf fmt "= %s" default in
  match param with
    | Param param ->
        dprintf "%s%a"
          param.name
          (pp_print_option pp_default) param.default
