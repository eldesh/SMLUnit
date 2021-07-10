
structure TestTestPath =
struct

  (***************************************************************************)

  structure Path = SMLUnit.TestPath

  (***************************************************************************)

  (**
   * perform tests
   *)
  fun runTest () =
    raise Fail "TestTestPath.runTest"

end

