(* Keywords *)

open Format

open Common

type keywords =
  | KeywordsIeee1800_2012 of { loc: Loc.t }

let keywords_ieee_1800_2012 loc = KeywordsIeee1800_2012 { loc }

let pp_keywords keywords = match keywords with
  | KeywordsIeee1800_2012 _ -> dprintf "1800-2012"
