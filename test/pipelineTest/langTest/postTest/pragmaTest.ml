(* Pragmas *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let pragma_value_exprs ?loc:(loc = LocTest.gen ()) ?exprs:(exprs = []) _ =
  Post.pragma_value_exprs loc exprs

let pragma_value_num ?loc:(loc = LocTest.gen ()) ?value:(value = ValueTest.value ()) _ =
  Post.pragma_value_num loc value

let pragma_value_string ?loc:(loc = LocTest.gen ()) ?value:(value = ValueTest.value ()) _ =
  Post.pragma_value_string loc value

let pragma_value_ident ?loc:(loc = LocTest.gen ()) ?name:(name = NameTest.name ()) _ =
  Post.pragma_value_ident loc name

let pragma_expr ?loc:(loc = LocTest.gen ()) ?kwd:(kwd = None) ?value:(value = None) _ =
  Post.pragma_expr loc kwd value

(* Assertions *)

let rec assert_pragma_value_equal ~ctxt expected actual = match expected, actual with
  | Post.PragmaValueExprs expected, Post.PragmaValueExprs actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (assert_pragma_expr_equal ~ctxt) expected.exprs actual.exprs
  | Post.PragmaValueNum expected, Post.PragmaValueNum actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    ValueTest.assert_value_equal ~ctxt expected.value actual.value
  | Post.PragmaValueString expected, Post.PragmaValueString actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    ValueTest.assert_value_equal ~ctxt expected.value actual.value
  | Post.PragmaValueIdent expected, Post.PragmaValueIdent actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.name actual.name
  | _ -> assert_failure "Pragma values are not equal"

and assert_pragma_expr_equal ~ctxt expected actual = match expected, actual with
  | Post.PragmaExpr expected, Post.PragmaExpr actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_optional_equal ~ctxt "keywords" NameTest.assert_name_equal expected.kwd actual.kwd;
    assert_optional_equal ~ctxt "values" assert_pragma_value_equal expected.value actual.value

(* Constructors *)

let fail_pragma_value_expected expected actual =
  str_formatter
    |> dprintf "Pragma values are not equal: expected %s, found %t" expected (Post.pp_pragma_value actual)
    |> flush_str_formatter
    |> assert_failure

let test_pragma_value_exprs ctxt =
  let loc = LocTest.gen () in
  let exprs = [
    pragma_expr
      ~kwd:(Some (NameTest.name ~name:"first" ()))
      ~value:(Some (pragma_value_num ~value:(ValueTest.value ~value:"42" ()) ()))
      ();
    pragma_expr
      ~kwd:(Some (NameTest.name ~name:"second" ()))
      ~value:(Some (pragma_value_string ~value:(ValueTest.value ~value:"a string" ()) ()))
      ();
  ] in
  match Post.pragma_value_exprs loc exprs with
    | Post.PragmaValueExprs actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      List.iter2 (assert_pragma_expr_equal ~ctxt) exprs actual.exprs
    | actual -> fail_pragma_value_expected "expressions" actual

let test_pragma_value_num ctxt =
  let loc = LocTest.gen () in
  let value = ValueTest.value () in
  match Post.pragma_value_num loc value with
    | Post.PragmaValueNum actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      ValueTest.assert_value_equal ~ctxt value actual.value
    | actual -> fail_pragma_value_expected "number" actual

let test_pragma_value_string ctxt =
  let loc = LocTest.gen () in
  let value = ValueTest.value () in
  match Post.pragma_value_string loc value with
    | Post.PragmaValueString actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      ValueTest.assert_value_equal ~ctxt value actual.value
    | actual -> fail_pragma_value_expected "string" actual

let test_pragma_value_ident ctxt =
  let loc = LocTest.gen () in
  let name = NameTest.name () in
  match Post.pragma_value_ident loc name with
    | Post.PragmaValueIdent actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt name actual.name
    | actual -> fail_pragma_value_expected "ident" actual

let fail_pragma_expr_expected expected actual =
  str_formatter
    |> dprintf "Pragma expressions are not equal: expected %s, found %t" expected (Post.pp_pragma_expr actual)
    |> flush_str_formatter
    |> assert_failure

let test_pragma_expr ctxt =
  let loc = LocTest.gen () in
  let kwd = Some (NameTest.name ()) in
  let value = Some (pragma_value_string ()) in
  match Post.pragma_expr loc kwd value with
    | Post.PragmaExpr actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_optional_equal ~ctxt "keywords" NameTest.assert_name_equal kwd actual.kwd;
      assert_optional_equal ~ctxt "values" assert_pragma_value_equal value actual.value

let constr =
  "Pragmas" >::: [
    "Values" >::: [
      "Expressions" >:: test_pragma_value_exprs;
      "Numbers"     >:: test_pragma_value_num;
      "Strings"     >:: test_pragma_value_string;
      "Identifiers" >:: test_pragma_value_ident;
    ];
    "Expressions" >:: test_pragma_expr;
  ]

(* Pretty Printing *)

let assert_pp_pragma_value = assert_pp Post.pp_pragma_value
let assert_pp_pragma_expr = assert_pp Post.pp_pragma_expr

let test_pp_pragma_value_exprs ctxt =
  let expr =
    pragma_expr
      ~kwd:(Some (NameTest.name ~name:"first" ()))
      ~value:(Some (pragma_value_string
          ~value:(ValueTest.value ~value:"first-value" ())
          ()))
      ()
  in
  let expr' =
    pragma_expr
      ~kwd:(Some (NameTest.name ~name:"second" ()))
      ~value:(Some (pragma_value_num
          ~value:(ValueTest.value ~value:"second-value" ())
          ()))
      ()
  in
  let exprs = [expr; expr'] in
  ()
    |> pragma_value_exprs ~exprs
    |> assert_pp_pragma_value ~ctxt [
         fprintf str_formatter "(%t, %t)"
           (Post.pp_pragma_expr expr)
           (Post.pp_pragma_expr expr')
           |> flush_str_formatter
       ]

let test_pp_pragma_value_num ctxt =
  let value = ValueTest.value () in
  ()
    |> pragma_value_num ~value
    |> assert_pp_pragma_value ~ctxt [
         fprintf str_formatter "%t"
           (Post.pp_value value)
           |> flush_str_formatter
       ]

let test_pp_pragma_value_string ctxt =
  let value = ValueTest.value () in
  ()
    |> pragma_value_string ~value
    |> assert_pp_pragma_value ~ctxt [
         fprintf str_formatter "%t"
           (Post.pp_value value)
           |> flush_str_formatter
       ]

let test_pp_pragma_value_ident ctxt =
  let name = NameTest.name () in
  ()
    |> pragma_value_ident ~name
    |> assert_pp_pragma_value ~ctxt [
         fprintf str_formatter "%t"
           (Post.pp_name name)
           |> flush_str_formatter
       ]

let test_pp_pragma_expr ctxt =
  let kwd = NameTest.name () in
  let value = pragma_value_string () in
  ()
    |> pragma_expr
    |> assert_pp_pragma_expr ~ctxt [""];
  ()
    |> pragma_expr ~kwd:(Some kwd)
    |> assert_pp_pragma_expr ~ctxt [
         fprintf str_formatter "%t"
           (Post.pp_name kwd)
           |> flush_str_formatter
       ];
  ()
    |> pragma_expr ~value:(Some value)
    |> assert_pp_pragma_expr ~ctxt [
         fprintf str_formatter "%t"
           (Post.pp_pragma_value value)
           |> flush_str_formatter
       ];
  ()
    |> pragma_expr ~kwd:(Some kwd) ~value:(Some value)
    |> assert_pp_pragma_expr ~ctxt [
         fprintf str_formatter "%t = %t"
           (Post.pp_name kwd)
           (Post.pp_pragma_value value)
           |> flush_str_formatter
       ]

let pp =
  "Pragmas" >::: [
    "Values" >::: [
      "Expressions" >:: test_pp_pragma_value_exprs;
      "Numbers"     >:: test_pp_pragma_value_num;
      "Strings"     >:: test_pp_pragma_value_string;
      "Identifiers" >:: test_pp_pragma_value_ident;
    ];
    "Expressions" >::  test_pp_pragma_expr;
  ]
