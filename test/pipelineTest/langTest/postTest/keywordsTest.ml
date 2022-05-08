(* Keywords *)

open OUnit2

open CommonTest

(* Fixtures *)

let keywords_ieee_1800_2012 ?loc:(loc = LocTest.gen ()) _ = Post.keywords_ieee_1800_2012 loc

(* Assertions *)

let assert_keywords_equal ~ctxt expected actual = match expected, actual with
  | Post.KeywordsIeee1800_2012 expected, Post.KeywordsIeee1800_2012 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc

(* Constructors *)

let test_keywords_ieee_1800_2012 ctxt =
  let loc = LocTest.gen () in
  match Post.keywords_ieee_1800_2012 loc with
    | Post.KeywordsIeee1800_2012 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc

let constr =
  "Keywords" >::: [
    "IEEE 1800-2012" >:: test_keywords_ieee_1800_2012;
  ]

(* Pretty Printing *)

let assert_pp_keywords = assert_pp Post.pp_keywords

let test_pp_keywords_ieee_1800_2012 ctxt =
  ()
    |> keywords_ieee_1800_2012
    |> assert_pp_keywords ~ctxt ["1800-2012"]

let pp =
  "Keywords" >::: [
    "IEEE 1800-2012" >:: test_pp_keywords_ieee_1800_2012;
  ]
