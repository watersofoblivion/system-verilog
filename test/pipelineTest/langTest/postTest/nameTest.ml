(* Names *)

open OUnit2

open CommonTest

(* Fixtures *)

let name ?loc:(loc = LocTest.gen ()) ?name:(name = "test-name") _ = Post.name loc name

(* Assertions *)

let assert_name_equal ~ctxt expected actual = match expected, actual with
  | Post.Name expected, Post.Name actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~msg:"Names are not equal" expected.name actual.name

(* Constructors *)

let test_name ctxt =
  let loc = LocTest.gen () in
  let name = "test-name" in
  match Post.name loc name with
    | Post.Name actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Names are not equal" name actual.name

let constr =
  "Names" >:: test_name

(** Pretty Printing *)

let assert_pp_name = assert_pp Post.pp_name

let test_pp_name ctxt =
  let n = "test-name" in
  ()
    |> name ~name:n
    |> assert_pp_name ~ctxt [n]

let pp =
  "Names" >:: test_pp_name
