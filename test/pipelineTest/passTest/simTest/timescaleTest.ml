(* Timescale *)

open Format

open Sim

open OUnit2

open CommonTest

(* Helpers *)

let printer pp x =
  x
    |> pp
    |> fprintf str_formatter "%t"
    |> flush_str_formatter

(* Fixtures *)

let magnitudes = [
  Timescale.one;
  Timescale.ten;
  Timescale.hundred;
]

let units = [
  Timescale.fs;
  Timescale.ps;
  Timescale.ns;
  Timescale.us;
  Timescale.ms;
  Timescale.s;
]

let scales =
  let map u =
    let map m = Timescale.scale m u in
    List.map map magnitudes
  in
  units
    |> List.map map
    |> List.flatten

let ts =
  let rec map = function
    | s :: ss ->
      let ts = Timescale.create s s in
      let tss = List.map (fun x -> Timescale.create x s) ss in
      (ts :: tss) :: (map ss)
    | [] -> []
  in
  scales
    |> map
    |> List.flatten

(* Assertions *)

let not_equal _ _ = false

let m_printer = printer Timescale.pp_m
let u_printer = printer Timescale.pp_u

let assert_m_equal ~ctxt expected actual = match expected, actual with
  | Timescale.One, Timescale.One
  | Timescale.Ten, Timescale.Ten
  | Timescale.Hundred, Timescale.Hundred -> ()
  | expected, actual ->
    assert_equal ~ctxt ~cmp:not_equal ~printer:m_printer ~msg:"Orders of magnitude are not equal" expected actual

let assert_u_equal ~ctxt expected actual = match expected, actual with
  | Timescale.Sec, Timescale.Sec
  | Timescale.Milli, Timescale.Milli
  | Timescale.Micro, Timescale.Micro
  | Timescale.Nano, Timescale.Nano
  | Timescale.Pico, Timescale.Pico
  | Timescale.Femto, Timescale.Femto -> ()
  | expected, actual ->
    assert_equal ~ctxt ~cmp:not_equal ~printer:u_printer ~msg:"Units are not equal" expected actual

let assert_s_equal ~ctxt expected actual =
  assert_m_equal ~ctxt expected.Timescale.scale_oom actual.Timescale.scale_oom;
  assert_u_equal ~ctxt expected.scale_unit actual.scale_unit

let assert_t_equal ~ctxt expected actual =
  assert_s_equal ~ctxt expected.Timescale.time_unit actual.Timescale.time_unit;
  assert_s_equal ~ctxt expected.time_precision actual.time_precision

let assert_pp_m = assert_pp Timescale.pp_m
let assert_pp_u = assert_pp Timescale.pp_u
let assert_pp_s = assert_pp Timescale.pp_s
let assert_pp_t = assert_pp Timescale.pp

(* Constructors *)

let fail_unexpected_oom actual =
  actual
    |> Timescale.pp_m
    |> fprintf str_formatter "Unexpected order of magnitude: %t"
    |> flush_str_formatter
    |> assert_failure

let fail_unexpected_unit actual =
  actual
    |> Timescale.pp_u
    |> fprintf str_formatter "Unexpected unit: %t"
    |> flush_str_formatter
    |> assert_failure

let test_one _ =
  match Timescale.one with
    | Timescale.One -> ()
    | actual -> fail_unexpected_oom actual

let test_ten _ =
  match Timescale.ten with
    | Timescale.Ten -> ()
    | actual -> fail_unexpected_oom actual

let test_hundred _ =
  match Timescale.hundred with
    | Timescale.Hundred -> ()
    | actual -> fail_unexpected_oom actual

let test_s _ =
  match Timescale.s with
    | Timescale.Sec -> ()
    | actual -> fail_unexpected_unit actual

let test_ms _ =
  match Timescale.ms with
    | Timescale.Milli -> ()
    | actual -> fail_unexpected_unit actual

let test_us _ =
  match Timescale.us with
    | Timescale.Micro -> ()
    | actual -> fail_unexpected_unit actual

let test_ns _ =
  match Timescale.ns with
    | Timescale.Nano -> ()
    | actual -> fail_unexpected_unit actual

let test_ps _ =
  match Timescale.ps with
    | Timescale.Pico -> ()
    | actual -> fail_unexpected_unit actual

let test_fs _ =
  match Timescale.fs with
    | Timescale.Femto -> ()
    | actual -> fail_unexpected_unit actual

let test_scale ctxt =
  List.iter (fun oom ->
    List.iter (fun yoonit ->
      let actual = Timescale.scale oom yoonit in
      assert_m_equal ~ctxt oom actual.Timescale.scale_oom;
      assert_u_equal ~ctxt yoonit actual.scale_unit
    ) units
  ) magnitudes

let test_create ctxt =
  let rec test less = function
    | yoonit :: greater ->
      let iter prec =
        let actual = Timescale.create yoonit prec in
        assert_s_equal ~ctxt yoonit actual.Timescale.time_unit;
        assert_s_equal ~ctxt prec actual.time_precision
      in
      List.iter iter less;

      let actual = Timescale.create yoonit yoonit in
      assert_s_equal ~ctxt yoonit actual.Timescale.time_unit;
      assert_s_equal ~ctxt yoonit actual.time_precision;

      let iter prec =
        let exn = Timescale.InvalidTimescale { time_unit = yoonit; time_precision = prec } in
        assert_raises exn (fun _ ->
          Timescale.create yoonit prec)
      in
      List.iter iter greater;

      test (yoonit :: less) greater
    | [] -> ()
  in
  test [] scales

(* Pretty-Printing *)

let test_pp_one ctxt =
  Timescale.one
    |> assert_pp_m ~ctxt ["1"]

let test_pp_ten ctxt =
  Timescale.ten
    |> assert_pp_m ~ctxt ["10"]

let test_pp_hundred ctxt =
  Timescale.hundred
    |> assert_pp_m ~ctxt ["100"]

let test_pp_s ctxt =
  Timescale.s
    |> assert_pp_u ~ctxt ["s"]

let test_pp_ms ctxt =
  Timescale.ms
    |> assert_pp_u ~ctxt ["ms"]

let test_pp_us ctxt =
  Timescale.us
    |> assert_pp_u ~ctxt ["us"]

let test_pp_ns ctxt =
  Timescale.ns
    |> assert_pp_u ~ctxt ["ns"]

let test_pp_ps ctxt =
  Timescale.ps
    |> assert_pp_u ~ctxt ["ps"]

let test_pp_fs ctxt =
  Timescale.fs
    |> assert_pp_u ~ctxt ["fs"]

let test_pp_scale ctxt =
  Timescale.ns
    |> Timescale.scale Timescale.ten
    |> assert_pp_s ~ctxt ["10ns"]

let test_pp_t ctxt =
  let time_unit = Timescale.scale Timescale.one Timescale.ns in
  Timescale.ps
    |> Timescale.scale Timescale.hundred
    |> Timescale.create time_unit
    |> assert_pp_t ~ctxt ["1ns/100ps"]

(* Rounding *)

let test_places ctxt =
  let rec iter = function
    | (prec :: ss) as scales ->
      List.iteri (fun expected yoonit ->
        Timescale.create yoonit prec
          |> Timescale.places
          |> assert_equal ~ctxt ~printer:string_of_int ~msg:"Rounding places are not equal" expected
      ) scales;
      iter ss
    | [] -> ()
  in
  iter scales

(* Comparison *)

let assert_compares ~ctxt pp cmp =
  let printer = printer pp in
  let lt x y = cmp x y < 0 in
  let eq x y = cmp x y = 0 in
  let gt x y = cmp x y > 0 in
  let rec assert_compares less = function
    | hd :: tl ->
      List.iter (assert_equal ~ctxt ~cmp:gt ~printer ~msg:"Expected less than" hd) less;
      assert_equal ~ctxt ~cmp:eq ~printer ~msg:"Expected equal" hd hd;
      List.iter (assert_equal ~ctxt ~cmp:lt ~printer ~msg:"Expected greater than" hd) tl;
      assert_compares (hd :: less) tl
    | [] -> ()
  in
  assert_compares []

let test_compare_m ctxt = assert_compares ~ctxt Timescale.pp_m Timescale.compare_m magnitudes
let test_compare_u ctxt = assert_compares ~ctxt Timescale.pp_u Timescale.compare_u units
let test_compare ctxt = assert_compares ~ctxt Timescale.pp_s Timescale.compare scales

(* Equality *)

let assert_equality ~ctxt pp eq =
  let printer = printer pp in
  let rec assert_equality ne = function
    | hd :: tl ->
      let assert_not_equal x =
        let cmp x y = not (eq x y) in
        assert_equal ~ctxt ~cmp ~printer ~msg:"Expected values not to be equal" x hd
      in
      List.iter assert_not_equal ne;
      assert_equal ~ctxt ~cmp:eq ~printer ~msg:"Expected values to be equal" hd hd;
      List.iter assert_not_equal tl;
      assert_equality (hd :: ne) tl
    | [] -> ()
  in
  assert_equality []

let test_equal_m ctxt = assert_equality ~ctxt Timescale.pp_m Timescale.equal_m magnitudes
let test_equal_u ctxt = assert_equality ~ctxt Timescale.pp_u Timescale.equal_u units
let test_equal_s ctxt = assert_equality ~ctxt Timescale.pp_s Timescale.equal_s scales
let test_equal ctxt = assert_equality ~ctxt Timescale.pp Timescale.equal ts

(* Test Suite *)

let suite =
  "Timescale" >::: [
    "Constructors" >::: [
      "Orders of Magnitude" >::: [
        "Zero" >:: test_one;
        "One"  >:: test_ten;
        "Two"  >:: test_hundred;
      ];
      "Units" >::: [
        "Seconds"      >:: test_s;
        "Milliseconds" >:: test_ms;
        "Microseconds" >:: test_us;
        "Nanoseconds"  >:: test_ns;
        "Picoseconds"  >:: test_ps;
        "Femtoseconds" >:: test_fs;
      ];
      "Scales"     >:: test_scale;
      "Timescales" >:: test_create;
    ];
    "Pretty Printing" >::: [
      "Orders of Magnitude" >::: [
        "Zero" >:: test_pp_one;
        "One"  >:: test_pp_ten;
        "Two"  >:: test_pp_hundred;
      ];
      "Units" >::: [
        "Seconds"      >:: test_pp_s;
        "Milliseconds" >:: test_pp_ms;
        "Microseconds" >:: test_pp_us;
        "Nanoseconds"  >:: test_pp_ns;
        "Picoseconds"  >:: test_pp_ps;
        "Femtoseconds" >:: test_pp_fs;
      ];
      "Scales"     >:: test_pp_scale;
      "Timescales" >:: test_pp_t;
    ];
    "Rounding" >:: test_places;
    "Comparison" >::: [
      "Orders of Magnitude" >:: test_compare_m;
      "Units"               >:: test_compare_u;
      "Scales"              >:: test_compare;
    ];
    "Equality" >::: [
      "Orders of Magnitude" >:: test_equal_m;
      "Units"               >:: test_equal_u;
      "Scales"              >:: test_equal_s;
      "Timescales"          >:: test_equal;
    ];
  ]
