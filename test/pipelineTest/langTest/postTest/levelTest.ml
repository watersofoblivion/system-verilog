(* Line Levels *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let level_entered ?loc:(loc = LocTest.gen ()) _ = Post.level_entered loc
let level_exited ?loc:(loc = LocTest.gen ()) _ = Post.level_exited loc

(* Assertions *)

let assert_level_equal ~ctxt expected actual = match expected, actual with
  | Post.LevelEntered expected, Post.LevelEntered actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.LevelExited expected, Post.LevelExited actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | _ -> assert_failure "Line levels are not equal"

(* Constructors *)

let fail_expected expected actual =
  str_formatter
    |> dprintf "Line levels are not equal: expected %s, found %t" expected (Post.pp_level (Some actual))
    |> flush_str_formatter
    |> assert_failure

let test_level_entered ctxt =
  let loc = LocTest.gen () in
  match Post.level_entered loc with
    | Post.LevelEntered actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "entered" actual

let test_level_exited ctxt =
  let loc = LocTest.gen () in
  match Post.level_exited loc with
    | Post.LevelExited actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "exited" actual

let constr =
  "Line Levels" >::: [
    "Entered" >:: test_level_entered;
    "Exited"  >:: test_level_exited;
  ]

(* Pretty Printing *)

let assert_pp_level = assert_pp (fun x -> Post.pp_level (Some x))

let test_pp_level_entered ctxt =
  ()
    |> level_entered
    |> assert_pp_level ~ctxt ["1"]

let test_pp_level_exited ctxt =
  ()
    |> level_exited
    |> assert_pp_level ~ctxt ["2"]

let test_pp_level_none ctxt =
  None
    |> assert_pp ~ctxt Post.pp_level ["0"]

let pp =
  "Line Levels" >::: [
    "Present" >::: [
      "Entered" >:: test_pp_level_entered;
      "Exited"  >:: test_pp_level_exited;
    ];
    "Absent" >:: test_pp_level_none;
  ]
