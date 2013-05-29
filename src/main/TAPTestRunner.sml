(**
 * A test runner which makes report about result of test in TAP format.
 *
 * satisfy the TAP specification
 *     ( http://search.cpan.org/~petdance/Test-Harness/lib/Test/Harness/TAP.pod )
 *)
structure TAPTestRunner : sig
  include TESTRUNNER
  exception TODO of string
  exception SKIP of string
end =
struct
  
  type parameter = {output : TextIO.outstream}

  datatype test = datatype Test.test

  fun printTo (parameter : parameter) string =
      TextIO.output(#output parameter, string)

  val int = Int.toString
  val cat = String.concatWith " "

  datatype status = Ok      of int
                  | NotOk   of int
                  | BailOut of int

  exception TODO of string (* TAP directives *)
  exception SKIP of string (*                *)

  fun StatusToString (Ok i)      = cat["ok"    , int i]
    | StatusToString (NotOk i)   = cat["not ok", int i]
    | StatusToString (BailOut i) = cat["Bail out!", "("^(int i)^")"]

  (* TAP strcture *)
  type testResult = { status : status
                    , description : string
                    , diagnostic : string list
                    }

  (**
   * format outline
   * 
   * <status> # <description>\n
   * # <diagnostic>\n
   * # <diagnostic>\n
   * # <diagnostic>
   * 
   *)
  fun toString {status, description=desc, diagnostic=diag} =
    let
      val line = cat[StatusToString status, desc]
    in
      (* each diagnostic line start with '#' *)
      String.concatWith "\n# " (line::diag)
    end

  fun doTest parameter path test =
    let
      fun println s = printTo parameter (s^"\n")
      fun go i path (TestCase test) =
        ((
          test();
          (* success *)
          println (toString {status=Ok i, description="", diagnostic=[]})
         )
         (* special note is specified *)
         handle TODO msg =>
            ( println (toString {status=NotOk i
                                , description=cat["#", "TODO", msg]
                                , diagnostic=[String.concatWith "." (rev path)] }))
         | SKIP reason => 
            ( println (toString {status=Ok i
                                , description=cat["#", "SKIP", reason]
                                , diagnostic=[String.concatWith "." (rev path)] }))
         (* failure *)
         | Assert.Fail failure =>
            let
              val message =
                case failure of
                  Assert.GeneralFailure message => message
                | Assert.NotEqualFailure (expected, actual) =>
                  "expected:<" ^ expected ^">, actual:<" ^ actual ^ ">"
            in
              println (toString {status=NotOk i
                                , description=message
                                , diagnostic=[String.concatWith "." (rev path)] })
            end
          | error => 
            ( println (toString {status=BailOut i
                                , description=cat["exception:", exnName error, ":", exnMessage error]
                                , diagnostic=[String.concatWith "." (rev path)] })
            ))
        | go i path (TestLabel (label, test)) =
              go (i+1) (label::path) test
        | go i path (TestList tests) =
            (
              foldl (fn (t,n)=> (go n path t;n+1)) i tests;
              ()
            )
    in
      go 0 [] test
    end

  local
    fun count (TestCase _) = 1
      | count (TestLabel (_, t)) = count t
      | count (TestList xs) = foldl op+ 0 (map count xs)
  in
    (* 'plan' of TAP format
     * The 'plan' tells how many tests will be run, or how many tests have run.
     *)
    fun mkplan test =
      concat["0..", int (count test)]
  end

  fun runTest parameter test =
    let
      val print = printTo parameter
    in
      print ((mkplan test)^"\n");
      doTest parameter "" test
    end

end


