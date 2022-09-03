(**
 {1 Lexer}
 *)

val json_path : string -> Path.t
(**
  Parse a JSON path.

  @param s The string representation of the JSON path
  @return The parsed JSON path
  @since 1.0
*)

val shape_path : string -> Path.t
(**
  Parse a shape path.

  @param str The string representation of the shape path
  @return The parsed shape path
  @since 1.0
*)
