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
  in
  fun main (_: string, _: string list) =
      let
        val tests =
            TestList
                (TestRequiredModules.tests () @ TestOptionalModules.tests ())
      in SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} tests
       ; OS.Process.success
      end
  end

  fun test () =
    ignore (main (CommandLine.name(), CommandLine.arguments()))

end
