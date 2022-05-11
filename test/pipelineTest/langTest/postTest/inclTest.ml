(* Includes *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let incl_path ?loc:(loc = LocTest.gen ()) ?sys:(sys = false) ?path:(path = ValueTest.value ()) _ =
  Post.incl_path loc sys path

let incl_macro ?loc:(loc = LocTest.gen ()) ?name:(name = NameTest.name ()) ?args:(args = None) _ =
  Post.incl_macro loc name args

(* Assertions *)

let assert_incl_equal ~ctxt expected actual = match expected, actual with
  | Post.IncludePath expected, Post.IncludePath actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"System flags are not equal" expected.sys actual.sys;
    ValueTest.assert_value_equal ~ctxt expected.path actual.path
  | Post.IncludeMacro expected, Post.IncludeMacro actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.name actual.name;
    assert_optional_equal ~ctxt "args" MacroTest.assert_args_equal expected.args actual.args
  | _ -> assert_failure "Include sources are not equal"

(* Constructurs *)

let fail_expected expected actual =
  str_formatter
    |> dprintf "Include sources are not equal: expected %s, found %t" expected (Post.pp_incl actual)
    |> flush_str_formatter
    |> assert_failure

let test_incl_path ctxt =
  let loc = LocTest.gen () in
  let sys = true in
  let path = ValueTest.value () in
  match Post.incl_path loc sys path with
    | Post.IncludePath actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~printer:string_of_bool ~msg:"System flags are not equal" sys actual.sys;
      ValueTest.assert_value_equal ~ctxt path actual.path
    | actual -> fail_expected "path" actual

let test_incl_macro ctxt =
  let loc = LocTest.gen () in
  let name = NameTest.name () in
  let args = Some (MacroTest.args ~args:[ValueTest.value ~value:"first" (); ValueTest.value ~value:"second" ()] ()) in
  match Post.incl_macro loc name args with
    | Post.IncludeMacro actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt name actual.name;
      assert_optional_equal ~ctxt "arguments" MacroTest.assert_args_equal args actual.args
    | actual -> fail_expected "macro" actual

let constr =
  "Include Sources" >::: [
    "Paths"  >:: test_incl_path;
    "Macros" >:: test_incl_macro;
  ]

(* Pretty Printing *)

let assert_pp_incl = assert_pp Post.pp_incl

let test_pp_incl_path ctxt =
  let path = ValueTest.value () in
  ()
    |> incl_path ~sys:true ~path
    |> assert_pp_incl ~ctxt [
         fprintf str_formatter "<%t>"
           (Post.pp_value path)
           |> flush_str_formatter
       ];
  ()
    |> incl_path ~sys:false ~path
    |> assert_pp_incl ~ctxt [
         fprintf str_formatter "\"%t\""
           (Post.pp_value path)
           |> flush_str_formatter
       ]

let test_pp_incl_macro ctxt =
  let name = NameTest.name () in
  let args = MacroTest.args ~args:[
    ValueTest.value ~value:"first" ();
    ValueTest.value ~value:"second" ()
  ] () in
  ()
    |> incl_macro ~name ~args:None
    |> assert_pp_incl ~ctxt [
         fprintf str_formatter "`%t"
           (Post.pp_name name)
           |> flush_str_formatter
       ];
  ()
    |> incl_macro ~name ~args:(Some args)
    |> assert_pp_incl ~ctxt [
         fprintf str_formatter "`%t(%t)"
           (Post.pp_name name)
           (Post.pp_args args)
           |> flush_str_formatter
       ]

let pp =
  "Include Sources" >::: [
    "Paths"  >:: test_pp_incl_path;
    "Macros" >:: test_pp_incl_macro;
  ]
