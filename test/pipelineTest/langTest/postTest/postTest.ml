open OUnit2

include NameTest
include ValueTest
include MacroTest
include TimescaleTest
include NetTest
include DriveTest
include PragmaTest
include LevelTest
include KeywordsTest
include DirArgSegTest

let suite =
  "Post-Processed Compilation Unit" >::: [
    "Constructors" >::: [
      NameTest.constr;
      ValueTest.constr;
      MacroTest.constr;
      TimescaleTest.constr;
      NetTest.constr;
      DriveTest.constr;
      PragmaTest.constr;
      LevelTest.constr;
      KeywordsTest.constr;
      DirArgSegTest.constr_dir;
      DirArgSegTest.constr_args;
      DirArgSegTest.constr_seg;
      FileTest.constr;
      CompilationUnitTest.constr;
    ];
    "Pretty Printing" >::: [
      NameTest.pp;
      ValueTest.pp;
      MacroTest.pp;
      TimescaleTest.pp;
      NetTest.pp;
      DriveTest.pp;
      PragmaTest.pp;
      LevelTest.pp;
      KeywordsTest.pp;
      DirArgSegTest.pp_dir;
      DirArgSegTest.pp_args;
      DirArgSegTest.pp_seg;
      FileTest.pp;
      CompilationUnitTest.pp;
    ];
  ]
