(* Drives *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let drive_up ?loc:(loc = LocTest.gen ()) _ = Post.drive_up loc
let drive_down ?loc:(loc = LocTest.gen ()) _ = Post.drive_down loc

(* Assertions *)

let assert_drive_equal ~ctxt expected actual = match expected, actual with
  | Post.DriveUp expected, Post.DriveUp actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DriveDown expected, Post.DriveDown actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | _ -> assert_failure "Drive types are not equal"

(* Constructors *)

let fail_expected expected actual =
  str_formatter
    |> dprintf "Drives are not equal: expected %s, found %t" expected (Post.pp_drive actual)
    |> flush_str_formatter
    |> assert_failure

let test_drive_up ctxt =
  let loc = LocTest.gen () in
  match Post.drive_up loc with
    | Post.DriveUp actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "up" actual

let test_drive_down ctxt =
  let loc = LocTest.gen () in
  match Post.drive_down loc with
    | Post.DriveDown actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "down" actual

let constr =
  "Drives" >::: [
    "Up"   >:: test_drive_up;
    "Down" >:: test_drive_down;
  ]

(* Pretty Printing *)

let assert_pp_drive = assert_pp Post.pp_drive

let test_pp_drive_up ctxt =
  ()
    |> drive_up
    |> assert_pp_drive ~ctxt ["pull1"]

let test_pp_drive_down ctxt =
  ()
    |> drive_down
    |> assert_pp_drive ~ctxt ["pull0"]

let pp =
  "Drives" >::: [
    "Up"   >:: test_pp_drive_up;
    "Down" >:: test_pp_drive_down;
  ]
