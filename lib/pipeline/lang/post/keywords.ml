(* Keywords *)

open Format

open Common

type keywords =
  | Keywords1364_1995 of { loc: Loc.t }
  | Keywords1364_2001_NoConfig of { loc: Loc.t }
  | Keywords1364_2001 of { loc: Loc.t }
  | Keywords1364_2005 of { loc: Loc.t }
  | Keywords1800_2005 of { loc: Loc.t }
  | Keywords1800_2009 of { loc: Loc.t }
  | Keywords1800_2012 of { loc: Loc.t }

let keywords_1364_1995 loc = Keywords1364_1995 { loc }
let keywords_1364_2001_noconfig loc = Keywords1364_2001_NoConfig { loc }
let keywords_1364_2001 loc = Keywords1364_2001 { loc }
let keywords_1364_2005 loc = Keywords1364_2005 { loc }
let keywords_1800_2005 loc = Keywords1800_2005 { loc }
let keywords_1800_2009 loc = Keywords1800_2009 { loc }
let keywords_1800_2012 loc = Keywords1800_2012 { loc }

let pp_keywords keywords = match keywords with
  | Keywords1364_1995 _ -> dprintf "1364-1995"
  | Keywords1364_2001_NoConfig _ -> dprintf "1364-2001-noconfig"
  | Keywords1364_2001 _ -> dprintf "1364-2001"
  | Keywords1364_2005 _ -> dprintf "1364-2005"
  | Keywords1800_2005 _ -> dprintf "1800-2005"
  | Keywords1800_2009 _ -> dprintf "1800-2009"
  | Keywords1800_2012 _ -> dprintf "1800-2012"
