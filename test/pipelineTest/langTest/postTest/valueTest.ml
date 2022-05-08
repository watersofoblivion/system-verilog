(* Values *)

open OUnit2

open CommonTest

(* Fixtures *)

let value ?loc:(loc = LocTest.gen ()) ?value:(value = "test-value") _ = Post.value loc value

(* Assertions *)

let assert_value_equal ~ctxt expected actual = match expected, actual with
  | Post.Value expected, Post.Value actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~msg:"Values are not equal" expected.value actual.value

(* Constructors *)

let test_value ctxt =
  let loc = LocTest.gen () in
  let value = "test-value" in
  match Post.value loc value with
    | Post.Value actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Values are not equal" value actual.value

let constr =
  "Values" >:: test_value

(** Pretty Printing *)

let assert_pp_value = assert_pp Post.pp_value

let test_pp_value ctxt =
  let v = "test-value" in
  ()
    |> value ~value:v
    |> assert_pp_value ~ctxt [v]

let pp =
  "Values" >:: test_pp_value
