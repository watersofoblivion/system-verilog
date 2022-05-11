open OUnit2

include NameTest
include ValueTest
include MacroTest
include InclTest
include TimescaleTest
include NetTest
include DriveTest
include PragmaTest
include LevelTest
include KeywordsTest
include DirTest
include SegTest

let suite =
  "Post-Processed Compilation Unit" >::: [
    "Constructors" >::: [
      NameTest.constr;
      ValueTest.constr;
      MacroTest.constr;
      InclTest.constr;
      TimescaleTest.constr;
      NetTest.constr;
      DriveTest.constr;
      PragmaTest.constr;
      LevelTest.constr;
      KeywordsTest.constr;
      DirTest.constr;
      SegTest.constr;
      FileTest.constr;
      CompilationUnitTest.constr;
    ];
    "Pretty Printing" >::: [
      NameTest.pp;
      ValueTest.pp;
      MacroTest.pp;
      InclTest.pp;
      TimescaleTest.pp;
      NetTest.pp;
      DriveTest.pp;
      PragmaTest.pp;
      LevelTest.pp;
      KeywordsTest.pp;
      DirTest.pp;
      SegTest.pp;
      FileTest.pp;
      CompilationUnitTest.pp;
    ];
  ]
