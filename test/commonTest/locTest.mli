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

val span : Loc.t -> Loc.t -> Loc.t
(** [span src dst] constructs a location starting at the start position of [src]
    and ending at the end position of [dst]. *)

val span_from : (int * int * int) -> Loc.t -> Loc.t
(** [span_from src dst] constructs a location starting at the start position
    [src] and ending at the end position of [dst]. *)

val shift : (int * int * int) -> Loc.t -> Loc.t
(** [shift (lines, cols, off) loc] shifts the location [loc] by [lines] lines,
    [cols] columns, and [off] bytes. *)

(**
 * {2 Assertions}
 *)

val assert_loc_equal : ctxt:test_ctxt -> Loc.t -> Loc.t -> unit
(** [assert_loc_equal ~ctxt expected actual] asserts that the location
    [expected] is equal to the location [actual].  The [ctxt] is passed to all
    internal assertions. *)
