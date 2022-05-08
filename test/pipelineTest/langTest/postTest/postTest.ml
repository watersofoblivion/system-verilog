open OUnit2

include NameTest
include ValueTest
include ParamTest
include TimescaleTest
include NetTest
include DriveTest
include LevelTest
include KeywordsTest
include DirSegTest

let suite =
  "Post-Processed Compilation Unit" >::: [
    "Constructors" >::: [
      NameTest.constr;
      ValueTest.constr;
      ParamTest.constr;
      TimescaleTest.constr;
      NetTest.constr;
      DriveTest.constr;
      LevelTest.constr;
      KeywordsTest.constr;
      DirSegTest.constr_dir;
      DirSegTest.constr_seg;
      FileTest.constr;
      CompilationUnitTest.constr;
    ];
    "Pretty Printing" >::: [
      NameTest.pp;
      ValueTest.pp;
      ParamTest.pp;
      TimescaleTest.pp;
      NetTest.pp;
      DriveTest.pp;
      LevelTest.pp;
      KeywordsTest.pp;
      DirSegTest.pp_dir;
      DirSegTest.pp_seg;
      FileTest.pp;
      CompilationUnitTest.pp;
    ];
  ]
