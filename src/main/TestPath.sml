
structure TestPath :> TEST_PATH =
struct
  datatype path = End
                | (*@ /foo *)
                  Child of string * path
                | (*@ //foo *)
                  Descendant of string * path

  fun match path ss =
    case (path, ss)
      of (End, _) => true
       | (Child (pattern, path), label::ss) =>
           if pattern = label
           then match path ss
           else false
       | (* //path "" *)
         (Descendant (pattern, path), []) =>
           match (Child (pattern, path)) ss
       | (* //path [label,ss] *)
         (Descendant (pattern, path), label::ss) =>
           match (Child      (pattern, path)) (label::ss) orelse
           match (Descendant (pattern, path)) ss
       | _ => false

  fun ofString' fs =
    case fs
      of (""::f::fs) => Descendant (f, ofString' fs)
       | (f::fs)     => Child      (f, ofString' fs)
       | []          => End

  fun ofString ss =
    let
      val ""::fields = String.fields (fn c => c = #"/") ss
    in
      ofString' fields
    end

  fun toString End = "End"
    | toString (Child (node,path)) = "Child(" ^ node ^ "," ^ toString path ^ ")"
    | toString (Descendant (node,path)) = "Descendant(" ^ node ^ "," ^ toString
    path ^ ")"
(*
  fun toString End = ""
    | toString (Child (node,path)) = "/" ^ node ^ toString path
    | toString (Descendant (node,path)) = "//" ^ node ^ toString path
    *)
end

