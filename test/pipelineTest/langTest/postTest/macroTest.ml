(* Parameters *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let param ?loc:(loc = LocTest.gen ()) ?name:(name = NameTest.name ()) ?default:(default = None) _ =
  Post.param loc name default

let params ?loc:(loc = LocTest.gen ()) ?params:(params = []) _ =
  Post.params loc params

let elem_source ?loc:(loc = LocTest.gen ()) ?value:(value = ValueTest.value ()) _ =
  Post.elem_source loc value

let elem_var ?loc:(loc = LocTest.gen ()) ?name:(name = NameTest.name ()) _ =
  Post.elem_var loc name

let line ?loc:(loc = LocTest.gen ()) ?elems:(elems = []) _ =
  Post.line loc elems

let body ?loc:(loc = LocTest.gen ()) ?lines:(lines = []) _ =
  Post.body loc lines

(* Assertions *)

let assert_param_equal ~ctxt expected actual = match expected, actual with
  | Post.Param expected, Post.Param actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.name actual.name;
    assert_optional_equal ~ctxt "default value" ValueTest.assert_value_equal expected.default actual.default

let assert_params_equal ~ctxt expected actual = match expected, actual with
  | Post.Params expected, Post.Params actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (assert_param_equal ~ctxt) expected.params actual.params

let assert_elem_equal ~ctxt expected actual = match expected, actual with
  | Post.ElemSource expected, Post.ElemSource actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    ValueTest.assert_value_equal ~ctxt expected.value actual.value
  | Post.ElemVar expected, Post.ElemVar actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.name actual.name
  | _ -> assert_failure "Body elemens are not equal"

let assert_line_equal ~ctxt expected actual = match expected, actual with
  | Post.Line expected, Post.Line actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (assert_elem_equal ~ctxt) expected.elems actual.elems

let assert_body_equal ~ctxt expected actual = match expected, actual with
  | Post.Body expected, Post.Body actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (assert_line_equal ~ctxt) expected.lines actual.lines

(* Constructors *)

(* Parameters *)

let test_param ctxt =
  let loc = LocTest.gen () in
  let name = NameTest.name () in
  let default = Some (ValueTest.value ()) in
  match Post.param loc name default with
    | Post.Param actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt name actual.name;
      assert_optional_equal ~ctxt "default value" ValueTest.assert_value_equal default actual.default

(* Parameter Lists *)

let test_params ctxt =
  let loc = LocTest.gen () in
  let params = [
    param ~name:(NameTest.name ~name:"first" ()) ();
    param ~name:(NameTest.name ~name:"second" ()) ~default:(Some (ValueTest.value ())) ();
  ] in
  match Post.params loc params with
    | Post.Params actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      List.iter2 (assert_param_equal ~ctxt) params actual.params

(* Body Elements *)

let fail_elem_expected expected actual =
  str_formatter
    |> dprintf "Macro body line elements are not equal: expected %s, found %t" expected (Post.pp_elem actual)
    |> flush_str_formatter
    |> assert_failure

let test_elem_source ctxt =
  let loc = LocTest.gen () in
  let value = ValueTest.value () in
  match Post.elem_source loc value with
    | Post.ElemSource actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      ValueTest.assert_value_equal ~ctxt value actual.value
    | actual -> fail_elem_expected "source" actual

let test_elem_var ctxt =
  let loc = LocTest.gen () in
  let name = NameTest.name () in
  match Post.elem_var loc name with
    | Post.ElemVar actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt name actual.name
    | actual -> fail_elem_expected "variable" actual

(* Lines *)

let test_line ctxt =
  let loc = LocTest.gen () in
  let elems = [
    elem_source ();
    elem_var ();
  ] in
  match Post.line loc elems with
    | Post.Line actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      List.iter2 (assert_elem_equal ~ctxt) elems actual.elems

(* Bodyies *)

let test_body ctxt =
  let loc = LocTest.gen () in
  let lines = [
    line ~elems:[
      elem_source ~value:(ValueTest.value ~value:"first source" ()) ();
      elem_var ~name:(NameTest.name ~name:"x" ()) ()
    ] ();
    line ~elems:[
      elem_var ~name:(NameTest.name ~name:"y" ()) ();
      elem_source ~value:(ValueTest.value ~value:"second source" ()) ()
    ] ();
  ] in
  match Post.body loc lines with
    | Post.Body actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      List.iter2 (assert_line_equal ~ctxt) lines actual.lines

let constr =
  "Macros" >::: [
    "Parameters" >::: [
      "Definition" >:: test_param;
      "Lists"      >:: test_params;
    ];
    "Body" >::: [
      "Lines" >::: [
        "Elements" >::: [
          "Source Code"        >:: test_elem_source;
          "Variable Reference" >:: test_elem_var;
        ];
        "Line" >:: test_line;
      ];
      "Definition" >:: test_body;
    ];
  ]

(* Pretty Printing *)

let assert_pp_param = assert_pp Post.pp_param
let assert_pp_params = assert_pp Post.pp_params
let assert_pp_elem = assert_pp Post.pp_elem
let assert_pp_line = assert_pp Post.pp_line
let assert_pp_body = assert_pp Post.pp_body

let test_pp_param ctxt =
  let name = NameTest.name ~name:"name" () in
  let blank = ValueTest.value ~value:"" () in
  let default = ValueTest.value ~value:"default value" () in
  ()
    |> param ~name ~default:None
    |> assert_pp_param ~ctxt [
         fprintf str_formatter "%t" (Post.pp_name name) |> flush_str_formatter
       ];
  ()
    |> param ~name ~default:(Some blank)
    |> assert_pp_param ~ctxt [
         fprintf str_formatter "%t =" (Post.pp_name name) |> flush_str_formatter
       ];
  ()
    |> param ~name ~default:(Some default)
    |> assert_pp_param ~ctxt [
         fprintf str_formatter "%t = %t" (Post.pp_name name) (Post.pp_value default) |> flush_str_formatter
       ]

let test_pp_params ctxt =
  let p1 = param ~name:(NameTest.name ~name:"first" ()) () in
  let p2 =
    param
      ~name:(NameTest.name ~name:"second" ())
      ~default:(Some (ValueTest.value ~value:"second-default" ()))
      ()
  in
  ()
    |> params ~params:[p1; p2]
    |> assert_pp_params ~ctxt [
         fprintf str_formatter "%t, %t"
           (Post.pp_param p1)
           (Post.pp_param p2)
           |> flush_str_formatter
       ]

let test_pp_elem_source ctxt =
  let value = ValueTest.value () in
  ()
    |> elem_source ~value
    |> assert_pp_elem ~ctxt [
         fprintf str_formatter "%t"
           (Post.pp_value value)
           |> flush_str_formatter
       ]

let test_pp_elem_var ctxt =
  let name = NameTest.name () in
  ()
    |> elem_var ~name
    |> assert_pp_elem ~ctxt [
         fprintf str_formatter "%t"
           (Post.pp_name name)
           |> flush_str_formatter
       ]

let test_pp_line ctxt =
  let elem = elem_source ~value:(ValueTest.value ~value:"some source code" ()) () in
  let elem' = elem_var ~name:(NameTest.name ~name:"some_var" ()) () in
  ()
    |> line ~elems:[elem; elem']
    |> assert_pp_line ~ctxt [
         fprintf str_formatter "%t%t"
           (Post.pp_elem elem)
           (Post.pp_elem elem')
           |> flush_str_formatter
       ]

let test_pp_body ctxt =
  let l1 = line ~elems:[
      elem_source ~value:(ValueTest.value ~value:"some source code    " ()) ();
      elem_var ~name:(NameTest.name ~name:"some_var" ()) ();
      elem_source ~value:(ValueTest.value ~value:"    " ()) ()
    ] ()
  in
  let l2 = line ~elems:[
      elem_source ~value:(ValueTest.value ~value:"  some more source code  " ()) ();
      elem_var ~name:(NameTest.name ~name:"another_var" ()) ()
    ] ()
  in
  ()
    |> body
    |> assert_pp_body ~ctxt [""];
  ()
    |> body ~lines:[l1]
    |> assert_pp_body ~ctxt [
          fprintf str_formatter "%t"
            (Post.pp_line l1)
            |> flush_str_formatter
       ];
  ()
    |> body ~lines:[l1; l2]
    |> assert_pp_body ~ctxt [
          fprintf str_formatter "%t\\\n%t"
            (Post.pp_line l1)
            (Post.pp_line l2)
            |> flush_str_formatter
       ]

let pp =
  "Macros" >::: [
    "Parameters" >::: [
      "Definition" >:: test_pp_param;
      "Lists"      >:: test_pp_params;
    ];
    "Body" >::: [
      "Lines" >::: [
        "Elements" >::: [
          "Source Code"        >:: test_pp_elem_source;
          "Variable Reference" >:: test_pp_elem_var;
        ];
        "Line" >:: test_pp_line;
      ];
      "Definition" >:: test_pp_body;
    ];
  ]
