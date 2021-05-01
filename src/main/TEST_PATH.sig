(**
 * a datatype represents test case filters as a subset of XPath.
 *)
signature TEST_PATH =
sig
  (**
   * a datatype represents a subtree of a test tree
   *)
  type path

  (**
   * Root node or it's descendants
   *
   * This represents the entire node of a tree
   *)
  val root : path

  (**
   * checks if the target is included in the set represented by path.
   *
   * @params path target
   * @param path a set of nodes
   * @param target a specific path of a test node
   *)
  val match : path -> string list -> bool

  (**
   * parse the input string as subset of XPath.
   *)
  val fromString : string -> path option

  val toString : path -> string
end

