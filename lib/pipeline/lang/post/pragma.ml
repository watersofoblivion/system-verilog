(* Pragmas *)

open Format

open Common

type pragma_value =
  | PragmaValueExprs of { loc: Loc.t; exprs: pragma_expr list }
  | PragmaValueNum of { loc: Loc.t; value: Value.value }
  | PragmaValueString of { loc: Loc.t; value: Value.value }
  | PragmaValueIdent of { loc: Loc.t; name: Name.name }

and pragma_expr =
  | PragmaExpr of { loc: Loc.t; kwd: Name.name option; value: pragma_value option }

(* Constructors *)

let pragma_value_exprs loc exprs = PragmaValueExprs { loc; exprs }
let pragma_value_num loc value = PragmaValueNum { loc; value }
let pragma_value_string loc value = PragmaValueString { loc; value }
let pragma_value_ident loc name = PragmaValueIdent { loc; name }

let pragma_expr loc kwd value = PragmaExpr { loc; kwd; value }

(* Pretty Printing *)

let rec pp_pragma_value = function
  | PragmaValueExprs value ->
    let pp fmt v = fprintf fmt "%t" (pp_pragma_expr v) in
    let pp_sep fmt _ = fprintf fmt ", " in
    dprintf "(%a)" (pp_print_list ~pp_sep pp) value.exprs
  | PragmaValueNum value -> dprintf "%t" (Value.pp_value value.value)
  | PragmaValueString value -> dprintf "%t" (Value.pp_value value.value)
  | PragmaValueIdent value -> dprintf "%t" (Name.pp_name value.name)

and pp_pragma_expr = function
  | PragmaExpr expr ->
    match expr.kwd, expr.value with
      | None, None -> (fun _ -> ())
      | Some kwd, None -> dprintf "%t" (Name.pp_name kwd)
      | None, Some value -> dprintf "%t" (pp_pragma_value value)
      | Some kwd, Some value -> dprintf "%t = %t" (Name.pp_name kwd) (pp_pragma_value value)
