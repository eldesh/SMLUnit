(**
 * entry point of the test suite of SML Basis library.
 * @author YAMATODANI Kiyoshi
 * @copyright 2010, Tohoku University.
 * @version $Id: TestMain.sml,v 1.1.28.1 2010/05/11 07:08:04 kiyoshiy Exp $
 *)
structure TestMain =
struct

  local
    open SMLUnit.Test
    structure Path = SMLUnit.TestPath
  in
  fun main (_: string, args: string list) =
      let
        val tests =
            TestList
                (TestRequiredModules.tests () @ TestOptionalModules.tests ())
        val path =
          if null args
          then [Path.root]
          else map (valOf o Path.fromString) args
      in
        app (fn path => print(concat["path:", Path.toString path,"\n"])) path;
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} path tests;
        OS.Process.success
      end
  end

  fun test () =
    ignore (main (CommandLine.name(), CommandLine.arguments()))

end
