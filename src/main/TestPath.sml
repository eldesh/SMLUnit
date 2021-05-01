
structure TestPath :> TEST_PATH =
struct
  structure Node =
  struct
    datatype node = Name of string
                  | Wildcard

    exception NodeNameIsEmpty

    fun scan rd ss =
      case rd ss
        of SOME ("*", ss) => SOME (Wildcard, ss)
         | SOME ("" ,  _) => raise NodeNameIsEmpty
         | SOME (nam, ss) => SOME (Name nam, ss)
         | NONE           => NONE

    fun fromString ss =
      case scan (String.scan Substring.getc) (Substring.full ss)
        of SOME (r, _) => SOME r
         | NONE        => NONE

    fun toString (Name name) = name
      | toString Wildcard    = "*"
  end

  datatype path = End
                | (*@ /foo *)
                  Child of Node.node * path
                | (*@ //foo *)
                  Descendant of Node.node * path

  exception NodeNameIsEmpty of { path: string }

  fun match path ss =
    case (path, ss)
      of (End, _) => true
       | (Child (Node.Wildcard, path), label::ss) =>
           match path ss
       | (Child (Node.Name nam, path), label::ss) =>
           nam = label andalso match path ss
       | (* //* "" *)
         (Descendant (Node.Wildcard, path), []) =>
           match path []
       | (* //nam "" *)
         (Descendant (Node.Name nam, path), []) =>
           false
       | (* //path [label,ss] *)
         (Descendant (pattern, path), label::ss) =>
           match (Child      (pattern, path)) (label::ss) orelse
           match (Descendant (pattern, path)) ss
       | _ => false

  fun fromString' fs =
    case fs
      of (""::f::fs) =>
         (case (Node.fromString f, fromString' fs)
            of (SOME node, SOME path) => SOME (Descendant (node, path))
             | _                      => NONE)
       | (f::fs)     =>
         (case (Node.fromString f, fromString' fs)
            of (SOME node, SOME path) => SOME (Child      (node, path))
             | _                      => NONE)
       | []          => SOME End

  fun fromString ss =
    let
      (* The 1st character of ss should be '/' *)
      val ""::fields = String.fields (fn c => c = #"/") ss
    in
      fromString' fields
      handle Node.NodeNameIsEmpty => raise NodeNameIsEmpty { path = ss }
    end

  fun toString End = "End"
    | toString (Child (node,path)) = "Child(" ^ Node.toString node ^ "," ^ toString path ^ ")"
    | toString (Descendant (node,path)) = "Descendant(" ^ Node.toString node ^ "," ^ toString
    path ^ ")"
(*
  fun toString End = ""
    | toString (Child (node,path)) = "/" ^ node ^ toString path
    | toString (Descendant (node,path)) = "//" ^ node ^ toString path
    *)
end

