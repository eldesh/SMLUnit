(**
 * datatypes for test cases and utility operators for them.
 * @author YAMATODANI Kiyoshi
 * @copyright 2010, Tohoku University.
 * @version $Id: TEST.sig,v 1.2 2004/10/20 02:09:35 kiyoshiy Exp $
 *)
signature TEST =
sig
  
  (***************************************************************************)

  (**
   * the type of function which perform a test case
   *)
  type testFunction = unit -> unit

  (**
   * the type representing a test or aggregation of tests.
   *)
  datatype test =
           (**
            * a test case
            * @params test
            * @param test the function which performs the test
            *)
           TestCase of testFunction
           (**
            * a test case with label
            * @params (label, test)
            * @param label the name of the test
            * @param test the function which performs the test
            *)
         | Test of string * testFunction
           (**
            * aggregation of tests with labels
            * @params labeled_tests
            * @param list of labeled tests
            *)
         | TestLabel of (string * test) list
           (**
            * aggregation of tests
            * @params tests
            * @param tests a list of tests
            *)
         | TestList of test list

  (***************************************************************************)

  (**
   *  labels tests and aggregates them into a test.
   *
   * @params nameAndTester
   * @param nameAndTester list of pair of name and tester of a test
   * @return a test which aggregates the tests
   *)
  val labelTests :
      (
        (** the name of test *)
        string *
        (** the function which performs the test *)
        testFunction
      ) list -> test

  (***************************************************************************)

end
