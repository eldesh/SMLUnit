
signature TEST_PATH =
sig
  type path

  val match : path -> string list -> bool

  val ofString : string -> path
  val toString : path -> string
end

