open OUnit2

open Common

(**
 * {1 Location Tracking}
 *)

val suite : test
(** [suite] is the test suite *)

(**
 * {2 Utilities}
 *)

val dummy : Loc.t
(** [dummy] is a location guaranteed not to be in any file. *)

val gen : unit -> Loc.t
(** [gen ()] generates a new, unique location. *)

val make : (int * int * int) -> (int * int * int) -> Loc.t
(** [make (start_line, start_col, start_off) (end_line, end_col, end_off)]
    constructs a location starting at the position
    [(start_line, start_col, start_off)] and ending at the position
    [(end_line, end_col, end_off)]. *)

(**
 * {3 Cursors}
 *)

type cursor
(** A cursor *)

val cursor : unit -> cursor
(** [cursor _] constructs a new cursor at the starting position (Line 1, column
    and offset 0.) *)

val newline : cursor -> unit
(** [newline cursor] increments the line number of the current cursor by 1 and
    resets the column to 0. *)

val advance : cursor -> int -> unit
(** [advance cursor n] advances [cursor] by [n] bytes.  (Column and offset are
    both incremented by n.) *)

val capture : cursor -> int -> Loc.t
(** [capture cursor n] captures the [n] bytes after the current position of
    [cursor] as a location.  (I.e., sets a mark, advances [n], then calls
    {!span}.) *)

val lexeme : cursor -> string -> Loc.t
(** [lexeme cursor lexeme] captures the lexeme [lexeme] starting at the current
    position of the cursor.  (I.e., it {!capture}s [String.length lexeme]
    bytes.) *)

(**
 * {4 Marks}
 *)

type mark
(** A marked position *)

val mark : cursor -> mark
(** [mark cursor] constructs a mark at the current position of [cursor]. *)

val span : mark -> cursor -> Loc.t
(** [span mark cursor] constructs a location spanning the position of [mark]
    to the current position of [cursor]. *)

(**
 * {2 Assertions}
 *)

val assert_loc_equal : ctxt:test_ctxt -> Loc.t -> Loc.t -> unit
(** [assert_loc_equal ~ctxt expected actual] asserts that the location
    [expected] is equal to the location [actual].  The [ctxt] is passed to all
    internal assertions. *)
