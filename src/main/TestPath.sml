
structure TestPath :> TEST_PATH =
struct
  structure Node =
  struct
    datatype node =
        (**
         * @params pattern
         * @param glob pattern matches to file paths
         *)
        Name of string

    exception NodeNameIsEmpty

    fun match (Name pat) str =
      let
        val pat = Substring.full pat
        val str = Substring.full str
        fun go pat str =
          case (Substring.getc pat, Substring.getc str)
            of (NONE         , NONE      ) => true
             | (SOME(#"*",cs), NONE      ) =>
                 go cs str
             | (SOME(#"*",cs), SOME(s,ss)) =>
                 (* consume one charactor or ... *)
                 go pat  ss orelse
                 (* does not consume even one charactor *)
                 go cs  str
             | (SOME(   c,cs), SOME(s,ss)) =>
                 c = s andalso go cs ss
             | (            _,          _) => false
      in
        go pat str
      end

    fun scan rd ss =
      case rd ss
        of SOME ("" ,  _) => raise NodeNameIsEmpty
         | SOME (nam, ss) => SOME (Name nam, ss)
         | NONE           => NONE

    fun fromString ss =
      case scan (String.scan Substring.getc) (Substring.full ss)
        of SOME (r, _) => SOME r
         | NONE        => NONE

    fun toString (Name name) = name
  end

  datatype path = End
                | (*@ /foo *)
                  Child of Node.node * path
                | (*@ //foo *)
                  Descendant of Node.node * path

  exception NodeNameIsEmpty of { path: string }

  val root = Descendant (Node.Name "*", End)

  fun equals (path1, path2) =
    case (path1, path2)
      of (End, End) => true
       | (Child (node1,path1), Child (node2,path2)) =>
           node1 = node2 andalso equals (path1, path2)
       | (Descendant (node1, path1), Descendant (node2, path2)) =>
           node1 = node2 andalso equals (path1, path2)
       | (_, _) => false

  fun match path ss =
    case (path, ss)
      of (End, _) => true
       | (Child (node, path), label::ss) =>
           Node.match node label andalso match path ss
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

