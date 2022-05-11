(* Includes *)

open Format

open Common

type incl =
  | IncludePath of { loc: Loc.t; sys: bool; path: Value.value }
  | IncludeMacro of { loc: Loc.t; name: Name.name; args: Macro.args option }

let incl_path loc sys path = IncludePath { loc; sys; path }
let incl_macro loc name args = IncludeMacro { loc; name; args }

let pp_incl = function
  | IncludePath incl ->
    if incl.sys
    then dprintf "<%t>" (Value.pp_value incl.path)
    else dprintf "\"%t\"" (Value.pp_value incl.path)
  | IncludeMacro incl ->
    let pp fmt args = fprintf fmt "(%t)" (Macro.pp_args args) in
    dprintf "`%t%a"
      (Name.pp_name incl.name)
      (pp_print_option pp) incl.args
