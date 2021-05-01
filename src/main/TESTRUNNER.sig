(**
 * This module runs test cases and makes report of their results.
 * @author YAMATODANI Kiyoshi
 * @copyright 2010, Tohoku University.
 * @version $Id: TESTRUNNER.sig,v 1.2 2004/10/20 02:09:35 kiyoshiy Exp $
 *)
signature TESTRUNNER =
sig
  
  (***************************************************************************)

  (**
   * implementation specific parameter to runTest function.
   *)
  type parameter

  (**
   *  perform tests
   * @params parameter path test
   * @param parameter implementation specific parameter
   * @param path a subtree of the test to perform
   * @param test a tree of possible tests to perform
   *)
  val runTest : parameter -> TestPath.path -> Test.test -> unit

  (***************************************************************************)

end