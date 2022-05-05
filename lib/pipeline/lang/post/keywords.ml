(* Keywords *)

open Format

type keywords =
  | Ieee1800_2012

let keywords_ieee_1800_2012 = Ieee1800_2012

let pp_keywords keywords = match keywords with
  | Ieee1800_2012 -> dprintf "1800-2012"
