(**
 * A test runner which makes report about result of test in plain text format.
 * @author YAMATODANI Kiyoshi
 * @copyright 2010, Tohoku University.
 * @version $Id: TextUITestRunner.sml,v 1.4 2005/04/09 14:44:32 kiyoshiy Exp $
 *)
structure TextUITestRunner : TESTRUNNER =
struct
  
  (***************************************************************************)

  (** NOTE : In future version, this might be changed so that the verbose level
   * of printing results can be specified. *)
  type parameter = {output : TextIO.outstream}

  type test = Test.test

  type testResult =
       {
         (** the number of test performed *)
         testCount : int, 
         (** the number of test skipped *)
         skipCount : int,
         (** tests aborted by assertion failure *)
         failures : (string * string) list, 
         (** tests aborted by any exception raised *)
         errors : (string * exn) list 
       }

  val testResultUnit : testResult =
    {testCount = 0, skipCount = 0, failures = [], errors = []}

  fun testResultAppend (testA: testResult, testB: testResult) : testResult =
    {
      testCount = #testCount testA + (#testCount testB),
      skipCount = #skipCount testA + (#skipCount testB),
      failures = #failures testA @ (#failures testB),
      errors = #errors testA @ (#errors testB)
    }

  (***************************************************************************)

  val separator = "/"

  (***************************************************************************)

  fun printTo (parameter : parameter) string =
      (TextIO.output(#output parameter, string);
       TextIO.flushOut(#output parameter))

  fun println ss = (print ss; print "\n")

  fun path_to_string path = separator ^ String.concatWith separator path

  fun doTest parameter filter path test =
      let
        val print = printTo parameter
      in
        case test of
          (Test.TestCase test) =>
          (
           (* any of the filters match this test path *)
           if List.exists (fn f => TestPath.match f path) filter
           then
             ( (* println("match: " ^ path_to_string path); *)
               test();
               print ".";
               {testCount = 1, skipCount = 0, failures = [], errors = []}
             )
             handle Assert.Fail failure =>
                    let
                      val message =
                          case failure of
                            Assert.GeneralFailure message => message
                          | Assert.NotEqualFailure (expected, actual) =>
                            "expected:<" ^ expected ^">, actual:<" ^ actual ^ ">"
                    in
                      print "F";
                      {testCount = 1, skipCount = 0, failures = [(path_to_string path, message)], errors = []}
                    end
                  | error =>
                    (
                      print "E";
                      {testCount = 1, skipCount = 0, failures = [], errors = [(path_to_string path, error)]}
                    )
           else
             ( (* println("skip: " ^ path_to_string path); *)
               print "S";
               {testCount = 0, skipCount = 1, failures = [], errors = []}
             ))
        | (Test.Test (label, f)) =>
          doTest parameter filter (label::path) (Test.TestCase f)
        | (Test.TestLabel labeled_tests) =>
          foldl
            (fn ((l,t),r) =>
                testResultAppend (r, doTest parameter filter (l::path) t))
            testResultUnit
            labeled_tests
        | (Test.TestList tests) =>
          let
            fun runOneTest (test, (index, acc_result)) =
                let
                  val result =
                      doTest
                          parameter
                          filter
                          (path @ [Int.toString index]) test
                in
                  (
                    index + 1,
                    testResultAppend (result, acc_result)
                  )
                end
          in
            #2
                (foldl
                     runOneTest
                     (1, testResultUnit)
                     tests)
          end
      end

  fun printTestResult parameter ({testCount, skipCount, failures, errors} : testResult) =
      let
        val print = printTo parameter
        val message =
            ("tests = " ^ (Int.toString testCount) ^ ", ") ^
            ("skipped = " ^ (Int.toString skipCount) ^ ", ") ^
            ("failures = " ^ (Int.toString (List.length failures)) ^ ", ") ^
            ("errors = " ^ (Int.toString (List.length errors)))
      in
        print ("\n" ^ message ^ "\n");
        print "Failures:\n";
        app
        (fn (path, message) =>
            (print path; print  ": "; print message; print "\n"))
        failures;
        print  "Errors:\n";
        app
        (fn (path, exn) =>
            (print
             (path ^ ": uncaught exception " ^ exnName exn ^ "\n <<< "
              ^ exnMessage exn ^ " >>>\n")))
        errors
      end

  (***************************************************************************)

  fun runTest parameter filters test =
      printTestResult parameter (doTest parameter filters [] test)

  (***************************************************************************)

end
