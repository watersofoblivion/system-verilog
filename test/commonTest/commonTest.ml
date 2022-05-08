open OUnit2

include TestUtils
include PpTest

module LocTest = struct
  include LocTest
end

let suite =
  "Common" >::: [
    (* TimescaleTest.suite; *)
    LocTest.suite;
  ]
