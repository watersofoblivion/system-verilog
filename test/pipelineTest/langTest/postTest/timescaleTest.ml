(* Timescales *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let mag_1 ?loc:(loc = LocTest.gen ()) _ = Post.mag_1 loc
let mag_10 ?loc:(loc = LocTest.gen ()) _ = Post.mag_10 loc
let mag_100 ?loc:(loc = LocTest.gen ()) _ = Post.mag_100 loc

let unit_s ?loc:(loc = LocTest.gen ()) _ = Post.unit_s loc
let unit_ms ?loc:(loc = LocTest.gen ()) _ = Post.unit_ms loc
let unit_us ?loc:(loc = LocTest.gen ()) _ = Post.unit_us loc
let unit_ns ?loc:(loc = LocTest.gen ()) _ = Post.unit_ns loc
let unit_ps ?loc:(loc = LocTest.gen ()) _ = Post.unit_ps loc
let unit_fs ?loc:(loc = LocTest.gen ()) _ = Post.unit_fs loc

let scale ?loc:(loc = LocTest.gen ()) ?mag:(mag = mag_1 ()) ?yoonit:(yoonit = unit_ns ()) _ =
  Post.scale loc mag yoonit

(* Assertions *)

let assert_mag_equal ~ctxt expected actual = match expected, actual with
  | Post.Magnitude1 expected, Post.Magnitude1 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.Magnitude10 expected, Post.Magnitude10 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.Magnitude100 expected, Post.Magnitude100 actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | _ -> assert_failure "Magnitudes are not equal"

let assert_yoonit_equal ~ctxt expected actual = match expected, actual with
  | Post.UnitS expected, Post.UnitS actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.UnitMs expected, Post.UnitMs actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.UnitUs expected, Post.UnitUs actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.UnitNs expected, Post.UnitNs actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.UnitPs expected, Post.UnitPs actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.UnitFs expected, Post.UnitFs actual -> LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | _ -> assert_failure "Units are not equal"

let assert_scale_equal ~ctxt expected actual = match expected, actual with
  | Post.Scale expected, Post.Scale actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_mag_equal ~ctxt expected.mag actual.mag;
    assert_yoonit_equal ~ctxt expected.yoonit actual.yoonit

(* Constructors *)

(* Orders of Magnitude *)

let fail_mag_expected expected actual =
  str_formatter
    |> dprintf "Magnitudes are not equal: expected %s, found %t" expected (Post.pp_mag actual)
    |> flush_str_formatter
    |> assert_failure

let test_mag_1 ctxt =
  let loc = LocTest.gen () in
  match Post.mag_1 loc with
    | Post.Magnitude1 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_mag_expected "zero orders of magnitude" actual

let test_mag_10 ctxt =
  let loc = LocTest.gen () in
  match Post.mag_10 loc with
    | Post.Magnitude10 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_mag_expected "one order of magnitude" actual

let test_mag_100 ctxt =
  let loc = LocTest.gen () in
  match Post.mag_100 loc with
    | Post.Magnitude100 actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_mag_expected "two orders of magnitude" actual

(* Units *)

let fail_yoonit_expected expected actual =
  str_formatter
    |> dprintf "Units are not equal: expected %s, found %t" expected (Post.pp_yoonit actual)
    |> flush_str_formatter
    |> assert_failure

let test_unit_s ctxt =
  let loc = LocTest.gen () in
  match Post.unit_s loc with
    | Post.UnitS actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_yoonit_expected "seconds unit" actual

let test_unit_ms ctxt =
  let loc = LocTest.gen () in
  match Post.unit_ms loc with
    | Post.UnitMs actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_yoonit_expected "millisecond unit" actual

let test_unit_us ctxt =
  let loc = LocTest.gen () in
  match Post.unit_us loc with
    | Post.UnitUs actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_yoonit_expected "microsecond unit" actual

let test_unit_ns ctxt =
  let loc = LocTest.gen () in
  match Post.unit_ns loc with
    | Post.UnitNs actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_yoonit_expected "nanosecond unit" actual

let test_unit_ps ctxt =
  let loc = LocTest.gen () in
  match Post.unit_ps loc with
    | Post.UnitPs actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_yoonit_expected "picosecond unit" actual

let test_unit_fs ctxt =
  let loc = LocTest.gen () in
  match Post.unit_fs loc with
    | Post.UnitFs actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_yoonit_expected "femtosecond unit" actual

(* Scales *)

let fail_scale_expected expected actual =
  str_formatter
    |> dprintf "Scales are not equal: expected %s, found %t" expected (Post.pp_scale actual)
    |> flush_str_formatter
    |> assert_failure

let test_scale ctxt =
  let loc = LocTest.gen () in
  let mag = mag_1 () in
  let yoonit = unit_s () in
  match Post.scale loc mag yoonit with
    | Post.Scale actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_mag_equal ~ctxt mag actual.mag;
      assert_yoonit_equal ~ctxt yoonit actual.yoonit

let constr =
  "Timescales" >::: [
    "Orders of Magnitude" >::: [
      "Zero" >:: test_mag_1;
      "One"  >:: test_mag_10;
      "Two"  >:: test_mag_100;
    ];
    "Units" >::: [
      "Seconds"     >:: test_unit_s;
      "Millisecond" >:: test_unit_ms;
      "Microsecond" >:: test_unit_us;
      "Nanosecond"  >:: test_unit_ns;
      "Picosecond"  >:: test_unit_ps;
      "Femtosecond" >:: test_unit_fs;
    ];
    "Scales" >:: test_scale;
  ]

(* Pretty Printing *)

(* Orders of Magnitude *)

let assert_pp_mag = assert_pp Post.pp_mag

let test_pp_mag_1 ctxt =
  ()
    |> mag_1
    |> assert_pp_mag ~ctxt ["1"]

let test_pp_mag_10 ctxt =
  ()
    |> mag_10
    |> assert_pp_mag ~ctxt ["10"]

let test_pp_mag_100 ctxt =
  ()
    |> mag_100
    |> assert_pp_mag ~ctxt ["100"]

(* Time Units *)

let assert_pp_yoonit = assert_pp Post.pp_yoonit

let test_pp_unit_s ctxt =
  ()
    |> unit_s
    |> assert_pp_yoonit ~ctxt ["s"]

let test_pp_unit_ms ctxt =
  ()
    |> unit_ms
    |> assert_pp_yoonit ~ctxt ["ms"]

let test_pp_unit_us ctxt =
  ()
    |> unit_us
    |> assert_pp_yoonit ~ctxt ["us"]

let test_pp_unit_ns ctxt =
  ()
    |> unit_ns
    |> assert_pp_yoonit ~ctxt ["ns"]

let test_pp_unit_ps ctxt =
  ()
    |> unit_ps
    |> assert_pp_yoonit ~ctxt ["ps"]

let test_pp_unit_fs ctxt =
  ()
    |> unit_fs
    |> assert_pp_yoonit ~ctxt ["fs"]

(* Scales *)

let assert_pp_scale = assert_pp Post.pp_scale

let test_pp_scale ctxt =
  ()
    |> scale ~mag:(mag_100 ()) ~yoonit:(unit_ns ())
    |> assert_pp_scale ~ctxt ["100ns"]

let pp =
  "Timescales" >::: [
    "Orders of Magnitude" >::: [
      "Zero" >:: test_pp_mag_1;
      "One"  >:: test_pp_mag_10;
      "Two"  >:: test_pp_mag_100;
    ];
    "Units" >::: [
      "Seconds"     >:: test_pp_unit_s;
      "Millisecond" >:: test_pp_unit_ms;
      "Microsecond" >:: test_pp_unit_us;
      "Nanosecond"  >:: test_pp_unit_ns;
      "Picosecond"  >:: test_pp_unit_ps;
      "Femtosecond" >:: test_pp_unit_fs;
    ];
    "Scales" >:: test_pp_scale;
  ]
