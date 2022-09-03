open OUnit2

open Common

(**
 * {1 Location Tracking}
 *)

val suite : test
(**
  The unit test suite.

  @since 1.0
*)

(**
 * {2 Utilities}
 *)

val dummy : Loc.t
(**
  A location guaranteed not to be in any file.

  This location is a singleton, and is therefore not distinct when used in
  multiple places.  Only use when constructing values where the location does
  not affect the outcome of the test.

  @since 1.0
*)

val gen : unit -> Loc.t
(**
  Generate a new, unique location.

  Locations generated by this function are guaranteed to be globally unique and
  sequentially increasing, but do not align with the actual size of parsed
  values.  Only use when constructing values where the location needs to be
  distinct but not necessarily accurate for the test to be correct.

  @return A distinct location
  @since 1.0
*)

val make : (int * int * int) -> (int * int * int) -> Loc.t
(**
  Construct a location spanning two explicit postions.

  @param (start_line, start_col, start_off) The line, column, and offset of the
    starting poisition
  @param (end_line, end_col, end_off) The line, column, and offset of the ending
    poisition
  @return A location spanning the two positions
  @since 1.0
*)

val span : Loc.t -> Loc.t -> Loc.t
(**
  Construct a location spanning from the start position of an earlier location
  to the ending position of a later location.

  This is useful for constructing locations of complex expressions from the
  locations of their constituent sub-expressions.  For example, when
  constructing a binary operator expression such as [1 + 2], the location of the
  expression would span from the location of the [1] to the location of the [2].

  @param src The earlier location
  @param dst The later location
  @return A location spanning the two locations
  @since 1.0
*)

val span_from : (int * int * int) -> Loc.t -> Loc.t
(**
  Construct a location spanning from an explicit starting position to the
  ending position of a later location.

  This is useful when there's some syntax that may not be captured by the
  location of a node in the abstract syntax tree, such as the opening quotation
  mark of a string (in combination with the {!shift} function) or explicitly
  specifying the beginning of a file.

  @param (start_line, start_col, start_off) The line, column, and offset of the
    starting poisition
  @param dst The later location
  @return A location spanning from the starting position to the end of the later
    location
  @since 1.0
*)

val shift : (int * int * int) -> Loc.t -> Loc.t
(**
  Shift a location by a number of lines, columns, and offset bytes.

  This is useful for nudging locations left and right due to punctuation marks
  not captured in the abstract syntax tree, such as parenthesis.

  @param (lines, cols, off) The number of lines, columns, and offsets to shift
    the location by.  Affects both the starting and ending positions.
  @param loc The location to shift
  @return A location shifted by the given amount
  @since 1.0
*)

(**
 * {2 Assertions}
 *)

val assert_loc_equal : ctxt:test_ctxt -> Loc.t -> Loc.t -> unit
(**
  Assert that two locations are equal.

  The values are considered equal if the file names are equal and the starting
  and ending line, column, and offset values are equal.

  @param ctxt The testing context
  @param expected The expected location
  @param actual The actual location
  @raise Failure Raised when the two locations disagree on file name or on
    starting or ending line, column, or offset values
  @since 1.0
*)
