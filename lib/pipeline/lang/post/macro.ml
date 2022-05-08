(* Macro Parameters *)

open Format

open Common

type param =
  | Param of { loc: Loc.t; name: Name.name; default: Value.value option }

type params =
  | Params of { loc: Loc.t; params: param list }

type elem =
  | ElemSource of { loc: Loc.t; value: Value.value }
  | ElemVar of { loc: Loc.t; name: Name.name }

type line =
  | Line of { loc: Loc.t; elems: elem list }

type body =
  | Body of { loc: Loc.t; lines: line list }

let param loc name default = Param { loc; name; default }

let params loc params = Params { loc; params }

let elem_source loc value = ElemSource { loc; value }
let elem_var loc name = ElemVar { loc; name }

let line loc elems = Line { loc; elems }

let body loc lines = Body { loc; lines }

let pp_param param =
  let pp fmt value = match value with
    | Value.Value v ->
      if v.value = ""
      then fprintf fmt " ="
      else fprintf fmt " = %t" (Value.pp_value value)
  in
  match param with
    | Param param ->
        dprintf "%t%a"
          (Name.pp_name param.name)
          (pp_print_option pp) param.default

let pp_params params =
  let pp_sep fmt _ = fprintf fmt ", " in
  let pp fmt param = pp_param param fmt in
  match params with
    | Params params ->
      dprintf "%a"
        (pp_print_list ~pp_sep pp) params.params

let pp_elem = function
  | ElemSource body -> dprintf "%t" (Value.pp_value body.value)
  | ElemVar body -> dprintf "%t" (Name.pp_name body.name)

let pp_line line =
  let pp_sep _ _ = () in
  let pp fmt elem = pp_elem elem fmt in
  match line with
    | Line line -> dprintf "%a" (pp_print_list ~pp_sep pp) line.elems

let pp_body body =
  let pp_sep fmt _ = fprintf fmt "\\\n" in
  let pp fmt line = pp_line line fmt in
  match body with
    | Body body -> dprintf "%a" (pp_print_list ~pp_sep pp) body.lines
