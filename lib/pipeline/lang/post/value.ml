(* Values *)

open Format

open Common

type value =
  | Value of { loc: Loc.t; value: string }

let value loc value = Value { loc; value }

let pp_value = function
  | Value value -> dprintf "%s" value.value
