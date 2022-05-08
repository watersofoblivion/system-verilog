(* Keywords *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let keywords_1364_1995 ?loc:(loc = LocTest.gen ()) _ = Post.keywords_1364_1995 loc
let keywords_1364_2001_noconfig ?loc:(loc = LocTest.gen ()) _ = Post.keywords_1364_2001_noconfig loc
let keywords_1364_2001 ?loc:(loc = LocTest.gen ()) _ = Post.keywords_1364_2001 loc
let keywords_1364_2005 ?loc:(loc = LocTest.gen ()) _ = Post.keywords_1364_2005 loc
let keywords_1800_2005 ?loc:(loc = LocTest.gen ()) _ = Post.keywords_1800_2005 loc
let keywords_1800_2009 ?loc:(loc = LocTest.gen ()) _ = Post.keywords_1800_2009 loc
let keywords_1800_2012 ?loc:(loc = LocTest.gen ()) _ = Post.keywords_1800_2012 loc

(* Assertions *)

let assert_keywords_equal ~ctxt expected actual = match expected, actual with
  | Post.Keywords1364_1995 expected, Post.Keywords1364_1995 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.Keywords1364_2001_NoConfig expected, Post.Keywords1364_2001_NoConfig actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.Keywords1364_2001 expected, Post.Keywords1364_2001 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.Keywords1364_2005 expected, Post.Keywords1364_2005 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.Keywords1800_2005 expected, Post.Keywords1800_2005 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.Keywords1800_2009 expected, Post.Keywords1800_2009 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.Keywords1800_2012 expected, Post.Keywords1800_2012 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | _ -> assert_failure "Keywords are not equal"

(* Constructors *)

let fail_expected expected actual =
  str_formatter
    |> dprintf "Keywords are not equal: expected %s, found %t" expected (Post.pp_keywords actual)
    |> flush_str_formatter
    |> assert_failure

let test_keywords_1364_1995 ctxt =
  let loc = LocTest.gen () in
  match Post.keywords_1364_1995 loc with
    | Post.Keywords1364_1995 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "1364-1995" actual

let test_keywords_1364_2001_noconfig ctxt =
  let loc = LocTest.gen () in
  match Post.keywords_1364_2001_noconfig loc with
    | Post.Keywords1364_2001_NoConfig actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "1364-2001-noconfig" actual

let test_keywords_1364_2001 ctxt =
  let loc = LocTest.gen () in
  match Post.keywords_1364_2001 loc with
    | Post.Keywords1364_2001 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "1364-2001" actual

let test_keywords_1364_2005 ctxt =
  let loc = LocTest.gen () in
  match Post.keywords_1364_2005 loc with
    | Post.Keywords1364_2005 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "1364-2005" actual

let test_keywords_1800_2005 ctxt =
  let loc = LocTest.gen () in
  match Post.keywords_1800_2005 loc with
    | Post.Keywords1800_2005 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "1800-2005" actual

let test_keywords_1800_2009 ctxt =
  let loc = LocTest.gen () in
  match Post.keywords_1800_2009 loc with
    | Post.Keywords1800_2009 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "1800-2009" actual

let test_keywords_1800_2012 ctxt =
  let loc = LocTest.gen () in
  match Post.keywords_1800_2012 loc with
    | Post.Keywords1800_2012 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "1800-2012" actual

let constr =
  "Keywords" >::: [
    "1364-1995"          >:: test_keywords_1364_1995;
    "1364-2001-noconfig" >:: test_keywords_1364_2001_noconfig;
    "1364-2001"          >:: test_keywords_1364_2001;
    "1364-2005"          >:: test_keywords_1364_2005;
    "1800-2005"          >:: test_keywords_1800_2005;
    "1800-2009"          >:: test_keywords_1800_2009;
    "1800-2012"          >:: test_keywords_1800_2012;
  ]

(* Pretty Printing *)

let assert_pp_keywords = assert_pp Post.pp_keywords

let test_pp_keywords_1364_1995 ctxt =
  ()
    |> keywords_1364_1995
    |> assert_pp_keywords ~ctxt ["1364-1995"]

let test_pp_keywords_1364_2001 ctxt =
  ()
    |> keywords_1364_2001
    |> assert_pp_keywords ~ctxt ["1364-2001"]

let test_pp_keywords_1364_2001_noconfig ctxt =
  ()
    |> keywords_1364_2001_noconfig
    |> assert_pp_keywords ~ctxt ["1364-2001-noconfig"]

let test_pp_keywords_1364_2005 ctxt =
  ()
    |> keywords_1364_2005
    |> assert_pp_keywords ~ctxt ["1364-2005"]

let test_pp_keywords_1800_2005 ctxt =
  ()
    |> keywords_1800_2005
    |> assert_pp_keywords ~ctxt ["1800-2005"]

let test_pp_keywords_1800_2009 ctxt =
  ()
    |> keywords_1800_2009
    |> assert_pp_keywords ~ctxt ["1800-2009"]

let test_pp_keywords_1800_2012 ctxt =
  ()
    |> keywords_1800_2012
    |> assert_pp_keywords ~ctxt ["1800-2012"]

let pp =
  "Keywords" >::: [
    "1364-1995"          >:: test_pp_keywords_1364_1995;
    "1364-2001-noconfig" >:: test_pp_keywords_1364_2001_noconfig;
    "1364-2001"          >:: test_pp_keywords_1364_2001;
    "1364-2005"          >:: test_pp_keywords_1364_2005;
    "1800-2005"          >:: test_pp_keywords_1800_2005;
    "1800-2009"          >:: test_pp_keywords_1800_2009;
    "1800-2012"          >:: test_pp_keywords_1800_2012;
  ]
