(* Directives and Segments *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

(* Directives *)

let dir_reset_all ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_reset_all loc

let dir_include ?loc:(loc = LocTest.gen ()) ?sys:(sys = false) ?path:(path = Result.get_ok (Fpath.of_string "path/to/include")) _ =
  Post.dir_include loc sys path

let dir_define ?loc:(loc = LocTest.gen ()) ?name:(name = NameTest.name ()) ?params:(params = []) ?body:(body = None) _ =
  Post.dir_define loc name params body

let dir_undef ?loc:(loc = LocTest.gen ()) ?name:(name = NameTest.name ()) _ =
  Post.dir_undef loc name

let dir_undefine_all ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_undefine_all loc

let dir_macro ?loc:(loc = LocTest.gen ()) ?name:(name = NameTest.name ()) ?args:(args = []) _ =
  Post.dir_macro loc name args

let dir_if_def ?loc:(loc = LocTest.gen ()) ?macro:(macro = NameTest.name ()) _ =
  Post.dir_if_def loc macro

let dir_if_n_def ?loc:(loc = LocTest.gen ()) ?macro:(macro = NameTest.name ()) _ =
  Post.dir_if_n_def loc macro

let dir_els_if ?loc:(loc = LocTest.gen ()) ?macro:(macro = NameTest.name ()) _ =
  Post.dir_els_if loc macro

let dir_else ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_else loc

let dir_end_if ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_end_if loc

let dir_timescale ?loc:(loc = LocTest.gen ()) ?yoonit:(yoonit = TimescaleTest.scale ()) ?prec:(prec = None) _ =
  Post.dir_timescale loc yoonit prec

let dir_default_net_type ?loc:(loc = LocTest.gen ()) ?net:(net = None) _ =
  Post.dir_default_net_type loc net

let dir_unconnected_drive ?loc:(loc = LocTest.gen ()) ?drive:(drive = DriveTest.drive_up ()) _ =
  Post.dir_unconnected_drive loc drive

let dir_no_unconnected_drive ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_no_unconnected_drive loc

let dir_cell_define ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_cell_define loc

let dir_end_cell_define ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_end_cell_define loc

let dir_pragma _ = failwith "TODO"

let dir_line ?loc:(loc = LocTest.gen ()) ?number:(number = 0) ?path:(path = Result.get_ok (Fpath.of_string "path/to/original/source")) ?level:(level = None) _ =
  Post.dir_line loc number path level

let dir_FILE ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_FILE loc

let dir_LINE ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_LINE loc

let dir_begin_keywords ?loc:(loc = LocTest.gen ()) ?keywords:(keywords = KeywordsTest.keywords_ieee_1800_2012 ()) _ =
  Post.dir_begin_keywords loc keywords

let dir_end_keywords ?loc:(loc = LocTest.gen ()) _ =
  Post.dir_end_keywords loc

(* Segments *)

let seg_source ?loc:(loc = LocTest.gen ()) ?src:(src = "") _ =
  Post.seg_source loc src

let seg_directive ?loc:(loc = LocTest.gen ()) ?dir:(dir = dir_reset_all ()) _ =
  Post.seg_directive loc dir

(* Assertions *)

let rec assert_dir_equal ~ctxt expected actual = match expected, actual with
  | Post.DirResetAll expected, Post.DirResetAll actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DirInclude expected, Post.DirInclude actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"System flags are not equal" expected.sys actual.sys;
    assert_equal ~ctxt ~cmp:Fpath.equal ~printer:Fpath.to_string ~msg:"Paths are not equal" expected.path actual.path
  | Post.DirDefine expected, Post.DirDefine actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.name actual.name;
    List.iter2 (ParamTest.assert_param_equal ~ctxt) expected.params actual.params;
    assert_optional_equal ~ctxt "value" ValueTest.assert_value_equal expected.body actual.body
  | Post.DirUndef expected, Post.DirUndef actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.name actual.name
  | Post.DirUndefineAll expected, Post.DirUndefineAll actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DirMacro expected, Post.DirMacro actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.name actual.name;
    List.iter2 (assert_seg_equal ~ctxt) expected.args actual.args
  | Post.DirIfDef expected, Post.DirIfDef actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.macro actual.macro
  | Post.DirIfNDef expected, Post.DirIfNDef actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.macro actual.macro
  | Post.DirElsIf expected, Post.DirElsIf actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    NameTest.assert_name_equal ~ctxt expected.macro actual.macro
  | Post.DirElse expected, Post.DirElse actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DirEndIf expected, Post.DirEndIf actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DirTimescale expected, Post.DirTimescale actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    TimescaleTest.assert_scale_equal ~ctxt expected.yoonit actual.yoonit;
    assert_optional_equal ~ctxt "precision" TimescaleTest.assert_scale_equal expected.prec actual.prec
  | Post.DirDefaultNetType expected, Post.DirDefaultNetType actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_optional_equal ~ctxt "net type" NetTest.assert_net_equal expected.net actual.net
  | Post.DirUnconnectedDrive expected, Post.DirUnconnectedDrive actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    DriveTest.assert_drive_equal ~ctxt expected.drive actual.drive
  | Post.DirNoUnconnectedDrive expected, Post.DirNoUnconnectedDrive actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DirCellDefine expected, Post.DirCellDefine actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DirEndCellDefine expected, Post.DirEndCellDefine actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DirPragma expected, Post.DirPragma actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    failwith "TODO"
  | Post.DirLine expected, Post.DirLine actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Line numbers are not equal" expected.number actual.number;
    assert_equal ~ctxt ~cmp:Fpath.equal ~printer:Fpath.to_string ~msg:"Paths are not equal" expected.path actual.path;
    assert_optional_equal ~ctxt "line level" LevelTest.assert_level_equal expected.level actual.level
  | Post.DirFILE expected, Post.DirFILE actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DirLINE expected, Post.DirLINE actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Post.DirBeginKeywords expected, Post.DirBeginKeywords actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    KeywordsTest.assert_keywords_equal ~ctxt expected.keywords actual.keywords
  | Post.DirEndKeywords expected, Post.DirEndKeywords actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | _ -> assert_failure "Directives are not equal"

and assert_seg_equal ~ctxt expected actual = match expected, actual with
  | Post.SegSource expected, Post.SegSource actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~msg:"Sources are not equal" expected.src actual.src
  | Post.SegDirective expected, Post.SegDirective actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_dir_equal ~ctxt expected.dir actual.dir
  | _ -> assert_failure "Segments are not equal"

(* Constructurs *)

(* Directives *)

let fail_dir_expected expected actual =
  str_formatter
    |> dprintf "Directives are not equal: expected %s, found %t" expected (Post.pp_dir actual)
    |> flush_str_formatter
    |> assert_failure

let test_dir_reset_all ctxt =
  let loc = LocTest.gen () in
  match Post.dir_reset_all loc with
    | Post.DirResetAll actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "reset all" actual

let test_dir_include ctxt =
  let loc = LocTest.gen () in
  let sys = true in
  let path =
    "the/path"
      |> Fpath.of_string
      |> Result.get_ok
  in
  match Post.dir_include loc sys path with
    | Post.DirInclude actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~printer:string_of_bool ~msg:"System flags are not equal" sys actual.sys;
      assert_equal ~ctxt ~cmp:Fpath.equal ~printer:Fpath.to_string ~msg:"Paths are not equal" path actual.path
    | actual -> fail_dir_expected "include" actual

let test_dir_define ctxt =
  let loc = LocTest.gen () in
  let name = NameTest.name () in
  let params = [
    ParamTest.param
      ~name:(NameTest.name ~name:"first" ())
      ~default:(Some (ValueTest.value ~value:"first" ()))
      ();
    ParamTest.param
      ~name:(NameTest.name ~name:"second" ())
      ~default:None
      ();
  ] in
  let body = Some (ValueTest.value ()) in
  match Post.dir_define loc name params body with
    | Post.DirDefine actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt name actual.name;
      List.iter2 (ParamTest.assert_param_equal ~ctxt) params actual.params;
      assert_optional_equal ~ctxt "body" ValueTest.assert_value_equal body actual.body
    | actual -> fail_dir_expected "macro definition" actual

let test_dir_undef ctxt =
  let loc = LocTest.gen () in
  let name = NameTest.name () in
  match Post.dir_undef loc name with
    | Post.DirUndef actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt name actual.name
    | actual -> fail_dir_expected "undefine macro" actual

let test_dir_undefine_all ctxt =
  let loc = LocTest.gen () in
  match Post.dir_undefine_all loc with
    | Post.DirUndefineAll actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "undefine all" actual

let test_dir_macro ctxt =
  let loc = LocTest.gen () in
  let name = NameTest.name () in
  let args = [
    seg_source ~src:"the-first-arg" ();
    seg_directive ~dir:(dir_macro ()) ();
  ] in
  match Post.dir_macro loc name args with
    | Post.DirMacro actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt name actual.name;
      List.iter2 (assert_seg_equal ~ctxt) args actual.args
    | actual -> fail_dir_expected "macro use" actual

let test_dir_if_def ctxt =
  let loc = LocTest.gen () in
  let macro = NameTest.name () in
  match Post.dir_if_def loc macro with
    | Post.DirIfDef actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt macro actual.macro
    | actual -> fail_dir_expected "if defined" actual

let test_dir_if_n_def ctxt =
  let loc = LocTest.gen () in
  let macro = NameTest.name () in
  match Post.dir_if_n_def loc macro with
    | Post.DirIfNDef actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt macro actual.macro
    | actual -> fail_dir_expected "if not defined" actual

let test_dir_els_if ctxt =
  let loc = LocTest.gen () in
  let macro = NameTest.name () in
  match Post.dir_els_if loc macro with
    | Post.DirElsIf actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      NameTest.assert_name_equal ~ctxt macro actual.macro
    | actual -> fail_dir_expected "else if defined" actual

let test_dir_else ctxt =
  let loc = LocTest.gen () in
  match Post.dir_else loc with
    | Post.DirElse actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "else" actual

let test_dir_end_if ctxt =
  let loc = LocTest.gen () in
  match Post.dir_end_if loc with
    | Post.DirEndIf actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "end if" actual

let test_dir_timescale ctxt =
  let loc = LocTest.gen () in
  let yoonit = TimescaleTest.scale ~mag:(TimescaleTest.mag_1 ()) ~yoonit:(TimescaleTest.unit_ns ()) () in
  let prec = Some (TimescaleTest.scale ~mag:(TimescaleTest.mag_100 ()) ~yoonit:(TimescaleTest.unit_ps ()) ()) in
  match Post.dir_timescale loc yoonit prec with
    | Post.DirTimescale actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      TimescaleTest.assert_scale_equal ~ctxt yoonit actual.yoonit;
      assert_optional_equal ~ctxt "precision" TimescaleTest.assert_scale_equal prec actual.prec
    | actual -> fail_dir_expected "timescale" actual

let test_dir_default_net_type ctxt =
  let loc = LocTest.gen () in
  let net = Some (NetTest.net_wire ()) in
  match Post.dir_default_net_type loc net with
    | Post.DirDefaultNetType actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_optional_equal ~ctxt "net type" NetTest.assert_net_equal net actual.net
    | actual -> fail_dir_expected "default net type" actual

let test_dir_unconnected_drive ctxt =
  let loc = LocTest.gen () in
  let drive = DriveTest.drive_up () in
  match Post.dir_unconnected_drive loc drive with
    | Post.DirUnconnectedDrive actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      DriveTest.assert_drive_equal ~ctxt drive actual.drive
    | actual -> fail_dir_expected "drive unconnected pins" actual

let test_dir_no_unconnected_drive ctxt =
  let loc = LocTest.gen () in
  match Post.dir_no_unconnected_drive loc with
    | Post.DirNoUnconnectedDrive actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "stop driving unconnected pins" actual

let test_dir_cell_define ctxt =
  let loc = LocTest.gen () in
  match Post.dir_cell_define loc with
    | Post.DirCellDefine actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "begin cell modules" actual

let test_dir_end_cell_define ctxt =
  let loc = LocTest.gen () in
  match Post.dir_end_cell_define loc with
    | Post.DirEndCellDefine actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "end cell modules" actual

let test_dir_pragma _ = failwith "TODO"

let test_dir_line ctxt =
  let loc = LocTest.gen () in
  let number = 42 in
  let path =
    "path/to/original/source"
      |> Fpath.of_string
      |> Result.get_ok
  in
  let level = Some (LevelTest.level_entered ()) in
  match Post.dir_line loc number path level with
    | Post.DirLine actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~printer:string_of_int ~msg:"Line numbers are not equal" number actual.number;
      assert_equal ~ctxt ~cmp:Fpath.equal ~printer:Fpath.to_string ~msg:"Paths are not equal" path actual.path;
      assert_optional_equal ~ctxt "line level" LevelTest.assert_level_equal level actual.level
    | actual -> fail_dir_expected "line level" actual

let test_dir_FILE ctxt =
  let loc = LocTest.gen () in
  match Post.dir_FILE loc with
    | Post.DirFILE actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "file name" actual

let test_dir_LINE ctxt =
  let loc = LocTest.gen () in
  match Post.dir_LINE loc with
    | Post.DirLINE actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "line number" actual

let test_dir_begin_keywords ctxt =
  let loc = LocTest.gen () in
  let keywords = KeywordsTest.keywords_ieee_1800_2012 () in
  match Post.dir_begin_keywords loc keywords with
    | Post.DirBeginKeywords actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      KeywordsTest.assert_keywords_equal ~ctxt keywords actual.keywords
    | actual -> fail_dir_expected "begin keywords" actual

let test_dir_end_keywords ctxt =
  let loc = LocTest.gen () in
  match Post.dir_end_keywords loc with
    | Post.DirEndKeywords actual -> LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> fail_dir_expected "end keywords" actual

let constr_dir =
  "Directives" >::: [
    "Reset All" >:: test_dir_reset_all;
    "Include"   >:: test_dir_include;
    "Macros"    >::: [
      "Define"       >:: test_dir_define;
      "Undefine"     >:: test_dir_undef;
      "Undefine All" >:: test_dir_undefine_all;
      "Use"          >:: test_dir_macro;
    ];
    "Conditionals" >::: [
      "If Defined"      >:: test_dir_if_def;
      "If Not Defined"  >:: test_dir_if_n_def;
      "Else If Defined" >:: test_dir_els_if;
      "Else"            >:: test_dir_else;
      "End"             >:: test_dir_end_if;
    ];
    "Timescale"        >:: test_dir_timescale;
    "Default Net Type" >:: test_dir_default_net_type;
    "Drive Unconnected Pins" >::: [
      "Set"   >:: test_dir_unconnected_drive;
      "Unset" >:: test_dir_no_unconnected_drive;
    ];
    "Cell Modules" >::: [
      "Start" >:: test_dir_cell_define;
      "End"   >:: test_dir_end_cell_define;
    ];
    "Pragmas"     >:: test_dir_pragma;
    "Line Levels" >:: test_dir_line;
    "Current Locations" >::: [
      "File Name"   >:: test_dir_FILE;
      "Line Number" >:: test_dir_LINE;
    ];
    "Keywords" >::: [
      "Begin" >:: test_dir_begin_keywords;
      "End"   >:: test_dir_end_keywords;
    ];
  ]

(* Segments *)

let fail_seg_expected expected actual =
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
    | actual -> fail_seg_expected "source code" actual

let test_seg_directive ctxt =
  let loc = LocTest.gen () in
  let dir = dir_reset_all () in
  match Post.seg_directive loc dir with
    | Post.SegDirective actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_dir_equal ~ctxt dir actual.dir
    | actual -> fail_seg_expected "compiler directive" actual

let constr_seg =
  "Segmets" >::: [
    "Source Code"        >:: test_seg_source;
    "Compiler Directive" >:: test_seg_directive;
  ]

(* Pretty Printing *)

(* Directives *)

let assert_pp_dir = assert_pp Post.pp_dir

let test_pp_dir_reset_all ctxt =
  ()
    |> dir_reset_all
    |> assert_pp_dir ~ctxt ["`resetall"]

let test_pp_dir_include ctxt =
  let path = Result.get_ok (Fpath.of_string "path/to/the/included/file") in
  ()
    |> dir_include ~sys:true ~path
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`include <%s>" (Fpath.to_string path)
           |> flush_str_formatter
       ];
  ()
    |> dir_include ~sys:false ~path
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`include %S" (Fpath.to_string path)
           |> flush_str_formatter
       ]

let test_pp_dir_define ctxt =
  let name = NameTest.name () in
  let param = ParamTest.param ~name:(NameTest.name ~name:"param-one" ()) () in
  let param' =
    ParamTest.param
      ~name:(NameTest.name ~name:"param-two" ())
      ~default:(Some (ValueTest.value ~value:"default-value" ()))
      ()
  in
  let params = [param; param'] in
  let single_line_body = ValueTest.value ~value:"a single line" () in
  let multi_line_body = ValueTest.value ~value:"a \nmulti-line\n  body" () in
  ()
    |> dir_define ~name
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`define %t" (Post.pp_name name)
           |> flush_str_formatter
       ];
  ()
    |> dir_define ~name ~params
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`define %t (%t, %t)"
           (Post.pp_name name)
           (Post.pp_param param)
           (Post.pp_param param')
           |> flush_str_formatter
       ];
  ()
    |> dir_define ~name ~body:(Some single_line_body)
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`define %t (%t, %t) %t"
           (Post.pp_name name)
           (Post.pp_param param)
           (Post.pp_param param')
           (Post.pp_value single_line_body)
           |> flush_str_formatter
       ];
  ()
    |> dir_define ~name ~params ~body:(Some multi_line_body)
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`define %t (%t, %t) a \\\n   multi-line\\\n  body"
           (Post.pp_name name)
           (Post.pp_param param)
           (Post.pp_param param')
           |> flush_str_formatter
       ]

let test_pp_dir_undef ctxt =
  let name = NameTest.name () in
  ()
    |> dir_undef ~name
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`undef %t"
           (Post.pp_name name)
           |> flush_str_formatter
       ]

let test_pp_dir_undefine_all ctxt =
  ()
    |> dir_undefine_all
    |> assert_pp_dir ~ctxt ["`undefineall"]

let test_pp_dir_macro ctxt =
  let name = NameTest.name () in
  let arg = seg_source () in
  let arg' = seg_directive () in
  let args = [arg; arg'] in
  ()
    |> dir_macro ~name
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`%t"
           (Post.pp_name name)
           |> flush_str_formatter
       ];
  ()
    |> dir_macro ~name ~args:[arg]
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`%t (%t)"
           (Post.pp_name name)
           (Post.pp_seg arg)
           |> flush_str_formatter
       ];
  ()
    |> dir_macro ~name ~args
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`%t (%t, %t)"
           (Post.pp_name name)
           (Post.pp_seg arg)
           (Post.pp_seg arg')
           |> flush_str_formatter
       ]

let test_pp_dir_if_def ctxt =
  let macro = NameTest.name () in
  ()
    |> dir_if_def ~macro
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`ifdef %t"
           (Post.pp_name macro)
           |> flush_str_formatter
       ]

let test_pp_dir_if_n_def ctxt =
  let macro = NameTest.name () in
  ()
    |> dir_if_n_def ~macro
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`ifndef %t"
           (Post.pp_name macro)
           |> flush_str_formatter
       ]

let test_pp_dir_els_if ctxt =
  let macro = NameTest.name () in
  ()
    |> dir_els_if ~macro
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`elsif %t"
           (Post.pp_name macro)
           |> flush_str_formatter
       ]

let test_pp_dir_else ctxt =
  ()
    |> dir_else
    |> assert_pp_dir ~ctxt ["`else"]

let test_pp_dir_end_if ctxt =
  ()
    |> dir_end_if
    |> assert_pp_dir ~ctxt ["`endif"]

let test_pp_dir_timescale ctxt =
  let yoonit =
    TimescaleTest.scale
      ~mag:(TimescaleTest.mag_1 ())
      ~yoonit:(TimescaleTest.unit_ns ())
      ()
  in
  let prec =
    TimescaleTest.scale
      ~mag:(TimescaleTest.mag_100 ())
      ~yoonit:(TimescaleTest.unit_ps ())
      ()
  in
  ()
    |> dir_timescale ~yoonit
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`timescale %t"
           (Post.pp_scale yoonit)
           |> flush_str_formatter
       ];
  ()
    |> dir_timescale ~yoonit ~prec:(Some prec)
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`timescale %t/%t"
           (Post.pp_scale yoonit)
           (Post.pp_scale prec)
           |> flush_str_formatter
       ]

let test_pp_dir_default_net_type ctxt =
  let net = Some (NetTest.net_tri_and ()) in
  ()
    |> dir_default_net_type ~net
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`default_nettype %t"
           (Post.pp_net net)
           |> flush_str_formatter
       ]

let test_pp_dir_unconnected_drive ctxt =
  let drive = DriveTest.drive_up () in
  ()
    |> dir_unconnected_drive ~drive
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`unconnected_drive %t"
           (Post.pp_drive drive)
           |> flush_str_formatter
       ]

let test_pp_dir_no_unconnected_drive ctxt =
  ()
    |> dir_no_unconnected_drive
    |> assert_pp_dir ~ctxt ["`no_unconnected_drive"]

let test_pp_dir_cell_define ctxt =
  ()
    |> dir_cell_define
    |> assert_pp_dir ~ctxt ["`celldefine"]

let test_pp_dir_end_cell_define ctxt =
  ()
    |> dir_end_cell_define
    |> assert_pp_dir ~ctxt ["`endcelldefine"]

let test_pp_dir_pragma _ = failwith "TODO"

let test_pp_dir_line ctxt =
  let number = 42 in
  let path = Result.get_ok (Fpath.of_string "path/to/the/original/source") in
  let level = Some (LevelTest.level_entered ()) in
  ()
    |> dir_line ~number ~path ~level
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`line %d %S %t"
           number
           (Fpath.to_string path)
           (Post.pp_level level)
           |> flush_str_formatter
    ]

let test_pp_dir_FILE ctxt =
  ()
    |> dir_FILE
    |> assert_pp_dir ~ctxt ["`__FILE__"]

let test_pp_dir_LINE ctxt =
  ()
    |> dir_LINE
    |> assert_pp_dir ~ctxt ["`__LINE__"]

let test_pp_dir_begin_keywords ctxt =
  let keywords = KeywordsTest.keywords_ieee_1800_2012 () in
  ()
    |> dir_begin_keywords ~keywords
    |> assert_pp_dir ~ctxt [
         fprintf str_formatter "`begin_keywords %t"
           (Post.pp_keywords keywords)
           |> flush_str_formatter
       ]

let test_pp_dir_end_keywords ctxt =
  ()
    |> dir_end_keywords
    |> assert_pp_dir ~ctxt ["`end_keywords"]

let pp_dir =
  "Directives" >::: [
    "Reset All" >:: test_pp_dir_reset_all;
    "Include"   >:: test_pp_dir_include;
    "Macros"    >::: [
      "Define"       >:: test_pp_dir_define;
      "Undefine"     >:: test_pp_dir_undef;
      "Undefine All" >:: test_pp_dir_undefine_all;
      "Use"          >:: test_pp_dir_macro;
    ];
    "Conditionals" >::: [
      "If Defined"      >:: test_pp_dir_if_def;
      "If Not Defined"  >:: test_pp_dir_if_n_def;
      "Else If Defined" >:: test_pp_dir_els_if;
      "Else"            >:: test_pp_dir_else;
      "End"             >:: test_pp_dir_end_if;
    ];
    "Timescale"        >:: test_pp_dir_timescale;
    "Default Net Type" >:: test_pp_dir_default_net_type;
    "Drive Unconnected Pins" >::: [
      "Set"   >:: test_pp_dir_unconnected_drive;
      "Unset" >:: test_pp_dir_no_unconnected_drive;
    ];
    "Cell Modules" >::: [
      "Start" >:: test_pp_dir_cell_define;
      "End"   >:: test_pp_dir_end_cell_define;
    ];
    "Pragmas"     >:: test_pp_dir_pragma;
    "Line Levels" >:: test_pp_dir_line;
    "Current Locations" >::: [
      "File Name"   >:: test_pp_dir_FILE;
      "Line Number" >:: test_pp_dir_LINE;
    ];
    "Keywords" >::: [
      "Begin" >:: test_pp_dir_begin_keywords;
      "End"   >:: test_pp_dir_end_keywords;
    ];
  ]

(* Segments *)

let assert_pp_seg = assert_pp Post.pp_seg

let test_pp_seg_source ctxt =
  let src = "the source code" in
  ()
    |> seg_source ~src
    |> assert_pp_seg ~ctxt [src]

let test_pp_seg_directive ctxt =
  let dir = dir_reset_all () in
  ()
    |> seg_directive ~dir
    |> assert_pp_seg ~ctxt [
         fprintf str_formatter "%t"
           (Post.pp_dir dir)
           |> flush_str_formatter
       ]

let pp_seg =
  "Segments" >::: [
    "Source Code"        >:: test_pp_seg_source;
    "Compiler Directive" >:: test_pp_seg_directive;
  ]
