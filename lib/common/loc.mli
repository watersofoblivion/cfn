(**
 * {1 Location Tracking}
 *)

type pos = private {
  line: int; (** Line *)
  col:  int; (** Column *)
  off:  int  (** Byte offset *)
}
(** A position *)

type t = private {
  start_pos : pos; (** Starting Position *)
  end_pos   : pos; (** Ending Position *)
}
(** A location *)

val loc : Lexing.position -> Lexing.position -> t
(** [loc start_pos end_pos] constructs a location spanning from the position
    [start_pos] to the position [end_pos]. *)
