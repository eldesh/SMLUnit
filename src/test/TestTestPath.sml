
structure TestTestPath =
struct

  (***************************************************************************)

  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert
  structure Path = SMLUnit.TestPath

  (***************************************************************************)

  exception TestFail of string

  (***************************************************************************)

  fun assertValidPath path =
    (case Path.fromString path
       of NONE => raise TestFail path
        | SOME _ => ())
     handle exn => raise TestFail path
 
  fun assertNotValidPath path =
    (case Path.fromString path
       of NONE => ()
        | SOME _ => raise TestFail path)
     handle exn => ()

  fun assertMatch path nodes =
    case Path.fromString path
      of NONE => raise TestFail (path ^ " " ^ String.concatWith "/" nodes)
       | SOME path =>
           if Path.match path nodes then ()
           else raise TestFail (Path.toString path ^ " " ^ String.concatWith "/" nodes)

  fun assertNotMatch path nodes =
    case Path.fromString path
      of NONE => raise TestFail (path ^ " " ^ String.concatWith "/" nodes)
       | SOME path =>
           if not (Path.match path nodes) then ()
           else raise TestFail (Path.toString path ^ " " ^ String.concatWith "/" nodes)


  (******************************************)

  fun testPathSyntax () =
    [
      assertValidPath "/*",
      assertValidPath "//*",
      assertValidPath "/foo",
      assertValidPath "/foo/bar",
      assertValidPath "/foo//bar",
      assertValidPath "/foo/*/bar",
      assertValidPath "/foo/*/bar",
      assertNotValidPath "/",
      assertNotValidPath "//",
      assertNotValidPath "*",
      assertNotValidPath "*/"
    ]

  fun testExactMatch () =
    [
      assertMatch "/foo" ["foo","bar","baz"],
      assertMatch "/foo/bar" ["foo","bar","baz"],
      assertMatch "/foo/bar/baz" ["foo","bar","baz"]
    ]

  fun testExactNotMatch () =
    [
      assertNotMatch "/fooo" ["foo","bar","baz"],
      assertNotMatch "/bar" ["foo","bar","baz"],
      assertNotMatch "/baz" ["foo","bar","baz"],
      assertNotMatch "/bar/baz" ["foo","bar","baz"],
      assertNotMatch "/bar/foo" ["foo","bar","baz"]
    ]

  fun testWildcardMatch () =
    [
      assertMatch "//*" ["foo","bar","baz"],
      assertMatch "//foo" ["foo","bar","baz"],
      assertMatch "//bar" ["foo","bar","baz"],
      assertMatch "//baz" ["foo","bar","baz"],
      assertMatch "//bar/baz" ["foo","bar","baz"],
      assertMatch "//bar//baz" ["foo","bar","baz"],
      assertMatch "/*/bar" ["foo","bar","baz"],
      assertMatch "/*/bar/baz" ["foo","bar","baz"],
      assertMatch "/foo/*/baz" ["foo","bar","baz"],
      assertMatch "/foo/*" ["foo","bar","baz"],
      assertMatch "/foo//*" ["foo","bar","baz"],
      assertMatch "/*/*/baz" ["foo","bar","baz"],
      assertMatch "/foo/*/*" ["foo","bar","baz"],
      assertMatch "/foo/*" ["foo","bar","baz"],
      assertMatch "/*/bar/*" ["foo","bar","baz"],
      assertMatch "//*/bar" ["foo","bar","baz"],
      assertMatch "//*/bar/*" ["foo","bar","baz"],
      assertMatch "//*/bar//*" ["foo","bar","baz"]
    ]

  fun testWildcardNotMatch () =
    [
      assertNotMatch "//*/foobar" ["foo","bar","baz"],
      assertNotMatch "/baz//*" ["foo","bar","baz"],
      assertNotMatch "/baz/*/bar" ["foo","bar","baz"]
    ]

  (******************************************)

  (**
   * perform tests
   *)
  fun runTest () =
      let
        val tests =
            [
              ("testPathSyntax"   , testPathSyntax),
              ("testExactMatch"   , testExactMatch),
              ("testExactNotMatch", testExactNotMatch),
              ("testWildcardMatch" , testWildcardMatch),
              ("testWildcardNotMatch", testWildcardNotMatch)
            ]
        val failCases =
            foldl
            (fn ((testName, testCase), failCases) =>
                ((testCase (); print "."; failCases)
                 handle TestFail data => (print "F"; (testName ^ ":" ^ data) :: failCases)
                      | exn => (print "E"; testName :: failCases)))
            []
            tests
      in
        print "\n";
        app (fn testName => (print testName; print "\n")) (List.rev failCases);
        print "\n"
      end

end

