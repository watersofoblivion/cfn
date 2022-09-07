(**
  Location Tracking
*)

type pos = private {
  line: int; (** Line *)
  col:  int; (** Column *)
  off:  int  (** Byte offset *)
}
(**
  A position within a file.

  @since 1.0
*)

type t = private {
  start_pos : pos; (** Starting Position *)
  end_pos   : pos; (** Ending Position *)
}
(**
  A location spaning two positions in a file.

  @since 1.0
*)

val loc : Lexing.position -> Lexing.position -> t
(**
  Construct a location spanning from a start position to an end position.

  @param start_pos The starting position
  @param end_pos The ending position
  @return A location spanning the positions
  @since 1.0
*)
