(**
 * test set for Basis modules which are classified into 'required'. 
 * @author YAMATODANI Kiyoshi
 * @copyright 2010, Tohoku University.
 * @version $Id: TestMain.sml,v 1.1.28.1 2010/05/11 07:08:04 kiyoshiy Exp $
 *)
structure TestRequiredModules =
struct

  local
    open SMLUnit.Test
  in
  fun tests () =
      let
        val tests =
          TestLabel
                [
                   ("Array001", Array001.suite ())
                  ,("Array101", Array101.suite ())
                  ,("ArraySlice001", ArraySlice001.suite ())
                  ,("ArraySlice101", ArraySlice101.suite ())
                  ,("Bool001", Bool001.suite ())
                  ,("Byte001", Byte001.suite ())
                  ,("Char001", Char001.suite ())
                  ,("CharArray001", CharArray001.suite ())
                  ,("CharArray101", CharArray101.suite ())
                  ,("CharArraySlice001", CharArraySlice001.suite ())
                  ,("CharArraySlice101", CharArraySlice101.suite ())
                  ,("CharVector001", CharVector001.suite ())
                  ,("CharVector101", CharVector101.suite ())
                  ,("CharVectorSlice001", CharVectorSlice001.suite ())
                  ,("CharVectorSlice101", CharVectorSlice101.suite ())
                  ,("Date001", Date001.suite ())
                  ,("General001", General001.suite ())
                  ,("IEEEReal001", IEEEReal001.suite ())
                  ,("Int001", Int001.suite ())
                  ,("LargeInt001", LargeInt001.suite ())
                  ,("LargeWord001", LargeWord001.suite ())
                  ,("List001", List001.suite ())
                  ,("ListPair001", ListPair001.suite ())
                  ,("Math001", Math001.suite ())
                  ,("Option001", Option001.suite ())
                  ,("Position001", Position001.suite ())
                  ,("Real001", Real001.suite ())
                  ,("String001", String001.suite ())
                  ,("StringCvt001", StringCvt001.suite ())
                  ,("Substring001", Substring001.suite ())
                  ,("Time001", Time001.suite ())
                  ,("Vector001", Vector001.suite ())
                  ,("Vector101", Vector101.suite ())
                  ,("VectorSlice001", VectorSlice001.suite ())
                  ,("VectorSlice101", VectorSlice101.suite ())
                  ,("Word001", Word001.suite ())
                  ,("Word8001", Word8001.suite ())
                  ,("Word8Array001", Word8Array001.suite ())
                  ,("Word8Array101", Word8Array101.suite ())
                  ,("Word8ArraySlice001", Word8ArraySlice001.suite ())
                  ,("Word8ArraySlice101", Word8ArraySlice101.suite ())
                  ,("Word8Vector001", Word8Vector001.suite ())
                  ,("Word8Vector101", Word8Vector101.suite ())
                  ,("Word8VectorSlice101", Word8VectorSlice101.suite ())
                  ,("Word8VectorSlice001", Word8VectorSlice001.suite ())
                ]
      in
        tests
      end
  end (* local *)

end
