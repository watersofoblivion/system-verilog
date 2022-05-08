(* Parameters *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let param ?loc:(loc = LocTest.gen ()) ?name:(name = NameTest.name ()) ?default:(default = None) _ =
  Post.param loc name default

(* Assertions *)

let assert_param_equal ~ctxt expected actual = match expected, actual with
  | Post.Param expected, Post.Param actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.name actual.name;
    assert_optional_equal ~ctxt "default value" ValueTest.assert_value_equal expected.default actual.default

(* Constructors *)

let test_param ctxt =
  let loc = LocTest.gen () in
  let name = NameTest.name () in
  let default = Some (ValueTest.value ()) in
  match Post.param loc name default with
    | Post.Param actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt name actual.name;
      assert_optional_equal ~ctxt "default value" ValueTest.assert_value_equal default actual.default

let constr =
  "Parameters" >:: test_param

(* Pretty Printing *)

let assert_pp_param = assert_pp Post.pp_param

let test_pp_param ctxt =
  let name = NameTest.name ~name:"name" () in
  let default = ValueTest.value ~value:"default value" () in
  ()
    |> param ~name ~default:None
    |> assert_pp_param ~ctxt [
         fprintf str_formatter "%t" (Post.pp_name name) |> flush_str_formatter
       ];
  ()
    |> param ~name ~default:(Some default)
    |> assert_pp_param ~ctxt [
         fprintf str_formatter "%t = %t" (Post.pp_name name) (Post.pp_value default) |> flush_str_formatter
       ]

let pp =
  "Parameters" >:: test_pp_param
