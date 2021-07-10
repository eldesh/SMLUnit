(**
 * test set for Basis modules which are classified into 'optional'. 
 * @author YAMATODANI Kiyoshi
 * @copyright 2010, Tohoku University.
 * @version $Id: TestMain.sml,v 1.1.28.1 2010/05/11 07:08:04 kiyoshiy Exp $
 *)
structure TestOptionalModules =
struct

  local
    open SMLUnit.Test
  in
  fun tests () =
    TestLabel
      [
        ("IntInf001", IntInf001.suite ()),
        ("IntInf101", IntInf101.suite ()),
        ("Array2001", Array2001.suite ()),
        ("RealArray001", RealArray001.suite ()),
        ("RealArray101", RealArray101.suite ()),
        ("RealVector001", RealVector001.suite ()),
        ("RealVector101", RealVector101.suite ())
      ]
  end (* local *)

end
