(* Segments *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let seg_source ?loc:(loc = LocTest.gen ()) ?src:(src = "") _ =
  Post.seg_source loc src

let seg_directive ?loc:(loc = LocTest.gen ()) ?dir:(dir = DirTest.dir_reset_all ()) _ =
  Post.seg_directive loc dir

(* Assertions *)

let assert_seg_equal ~ctxt expected actual = match expected, actual with
  | Post.SegSource expected, Post.SegSource actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~msg:"Sources are not equal" expected.src actual.src
  | Post.SegDirective expected, Post.SegDirective actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    DirTest.assert_dir_equal ~ctxt expected.dir actual.dir
  | _ -> assert_failure "Segments are not equal"

(* Constructurs *)

let fail_expected expected actual =
  str_formatter
    |> dprintf "Segments are not equal: expected %s, found %t" expected (Post.pp_seg actual)
    |> flush_str_formatter
    |> assert_failure

let test_seg_source ctxt =
  let loc = LocTest.gen () in
  let src = "the source code" in
  match Post.seg_source loc src with
    | Post.SegSource actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Sources are not equal" src actual.src
    | actual -> fail_expected "source code" actual

let test_seg_directive ctxt =
  let loc = LocTest.gen () in
  let dir = DirTest.dir_reset_all () in
  match Post.seg_directive loc dir with
    | Post.SegDirective actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      DirTest.assert_dir_equal ~ctxt dir actual.dir
    | actual -> fail_expected "compiler directive" actual

let constr =
  "Segmets" >::: [
    "Source Code"        >:: test_seg_source;
    "Compiler Directive" >:: test_seg_directive;
  ]

(* Pretty Printing *)

let assert_pp_seg = assert_pp Post.pp_seg

let test_pp_seg_source ctxt =
  let src = "the source code" in
  ()
    |> seg_source ~src
    |> assert_pp_seg ~ctxt [src]

let test_pp_seg_directive ctxt =
  let dir = DirTest.dir_reset_all () in
  ()
    |> seg_directive ~dir
    |> assert_pp_seg ~ctxt [
         fprintf str_formatter "%t"
           (Post.pp_dir dir)
           |> flush_str_formatter
       ]

let pp =
  "Segments" >::: [
    "Source Code"        >:: test_pp_seg_source;
    "Compiler Directive" >:: test_pp_seg_directive;
  ]
