(* Net Types *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let net_wire ?loc:(loc = LocTest.gen ()) _ = Post.net_wire loc
let net_tri ?loc:(loc = LocTest.gen ()) _ = Post.net_tri loc
let net_tri0 ?loc:(loc = LocTest.gen ()) _ = Post.net_tri0 loc
let net_tri1 ?loc:(loc = LocTest.gen ()) _ = Post.net_tri1 loc
let net_w_and ?loc:(loc = LocTest.gen ()) _ = Post.net_w_and loc
let net_tri_and ?loc:(loc = LocTest.gen ()) _ = Post.net_tri_and loc
let net_w_or ?loc:(loc = LocTest.gen ()) _ = Post.net_w_or loc
let net_tri_or ?loc:(loc = LocTest.gen ()) _ = Post.net_tri_or loc
let net_tri_reg ?loc:(loc = LocTest.gen ()) _ = Post.net_tri_reg loc
let net_u_wire ?loc:(loc = LocTest.gen ()) _ = Post.net_u_wire loc

(* Assertions *)

let assert_net_equal ~ctxt expected actual = match expected, actual with
  | Post.NetWire expected, Post.NetWire actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.NetTri expected, Post.NetTri actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.NetTri0 expected, Post.NetTri0 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.NetTri1 expected, Post.NetTri1 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.NetWAnd expected, Post.NetWAnd actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.NetTriAnd expected, Post.NetTriAnd actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.NetWOr expected, Post.NetWOr actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.NetTriOr expected, Post.NetTriOr actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.NetTriReg expected, Post.NetTriReg actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.NetUWire expected, Post.NetUWire actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | _, _ -> assert_failure "Net types are not equal"

(* Constructors *)

let fail_expected expected actual =
  str_formatter
    |> dprintf "Net types are not equal: expected %s, found %t" expected (Post.pp_net (Some actual))
    |> flush_str_formatter
    |> assert_failure

let test_net_wire ctxt =
  let loc = LocTest.gen () in
  match Post.net_wire loc with
    | Post.NetWire actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "wire" actual

let test_net_tri ctxt =
  let loc = LocTest.gen () in
  match Post.net_tri loc with
    | Post.NetTri actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "tri" actual

let test_net_tri0 ctxt =
  let loc = LocTest.gen () in
  match Post.net_tri0 loc with
    | Post.NetTri0 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "tri0" actual

let test_net_tri1 ctxt =
  let loc = LocTest.gen () in
  match Post.net_tri1 loc with
    | Post.NetTri1 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "tri1" actual

let test_net_w_and ctxt =
  let loc = LocTest.gen () in
  match Post.net_w_and loc with
    | Post.NetWAnd actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "wand" actual

let test_net_tri_and ctxt =
  let loc = LocTest.gen () in
  match Post.net_tri_and loc with
    | Post.NetTriAnd actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "triand" actual

let test_net_w_or ctxt =
  let loc = LocTest.gen () in
  match Post.net_w_or loc with
    | Post.NetWOr actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "wor" actual

let test_net_tri_or ctxt =
  let loc = LocTest.gen () in
  match Post.net_tri_or loc with
    | Post.NetTriOr actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "trior" actual

let test_net_tri_reg ctxt =
  let loc = LocTest.gen () in
  match Post.net_tri_reg loc with
    | Post.NetTriReg actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "trireg" actual

let test_net_u_wire ctxt =
  let loc = LocTest.gen () in
  match Post.net_u_wire loc with
    | Post.NetUWire actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_expected "uwire" actual

let constr =
  "Net Types" >::: [
    "Wire"   >:: test_net_wire;
    "Tri"    >:: test_net_tri;
    "Tri0"   >:: test_net_tri0;
    "Tri1"   >:: test_net_tri1;
    "WAnd"   >:: test_net_w_and;
    "TriAnd" >:: test_net_tri_and;
    "WOr"    >:: test_net_w_or;
    "TriOr"  >:: test_net_tri_or;
    "TriReg" >:: test_net_tri_reg;
    "UWire"  >:: test_net_u_wire;
  ]

(* Pretty Printing *)

let assert_pp_net = assert_pp (fun x -> Post.pp_net (Some x))

let test_pp_net_wire ctxt =
  ()
    |> net_wire
    |> assert_pp_net ~ctxt ["wire"]

let test_pp_net_tri ctxt =
  ()
    |> net_tri
    |> assert_pp_net ~ctxt ["tri"]

let test_pp_net_tri0 ctxt =
  ()
    |> net_tri0
    |> assert_pp_net ~ctxt ["tri0"]

let test_pp_net_tri1 ctxt =
  ()
    |> net_tri1
    |> assert_pp_net ~ctxt ["tri1"]

let test_pp_net_w_and ctxt =
  ()
    |> net_w_and
    |> assert_pp_net ~ctxt ["wand"]

let test_pp_net_tri_and ctxt =
  ()
    |> net_tri_and
    |> assert_pp_net ~ctxt ["triand"]

let test_pp_net_w_or ctxt =
  ()
    |> net_w_or
    |> assert_pp_net ~ctxt ["wor"]

let test_pp_net_tri_or ctxt =
  ()
    |> net_tri_or
    |> assert_pp_net ~ctxt ["trior"]

let test_pp_net_tri_reg ctxt =
  ()
    |> net_tri_reg
    |> assert_pp_net ~ctxt ["trireg"]

let test_pp_net_u_wire ctxt =
  ()
    |> net_u_wire
    |> assert_pp_net ~ctxt ["uwire"]

let test_pp_net_none ctxt =
  None
    |> assert_pp Post.pp_net ~ctxt ["none"]

let pp =
  "Net Types" >::: [
    "Wire"   >:: test_pp_net_wire;
    "Tri"    >:: test_pp_net_tri;
    "Tri0"   >:: test_pp_net_tri0;
    "Tri1"   >:: test_pp_net_tri1;
    "WAnd"   >:: test_pp_net_w_and;
    "TriAnd" >:: test_pp_net_tri_and;
    "WOr"    >:: test_pp_net_w_or;
    "TriOr"  >:: test_pp_net_tri_or;
    "TriReg" >:: test_pp_net_tri_reg;
    "UWire"  >:: test_pp_net_u_wire;
    "None"   >:: test_pp_net_none;
  ]
