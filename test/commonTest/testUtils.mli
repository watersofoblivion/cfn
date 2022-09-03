open Format

open OUnit2

(**
 * {1 Testing Utilities}
 *)

val not_equal : ctxt:test_ctxt -> string -> (formatter -> 'a -> unit) -> 'a -> 'a -> unit
(**
  Forces a test to fail because two values are not equal.

  @param ctxt The testing context
  @param msg The name of the type in the error message
  @param pp A pretty-printer for the values
  @param expected The expected value
  @param actual The actual value
  @raise Failure Always
  @since 1.0
*)

val assert_optional_equal : ctxt:test_ctxt -> string -> (ctxt:test_ctxt -> 'a -> 'a -> unit) -> 'a option -> 'a option -> unit
(**
  Assert two optional values are equal.

  The values are considered equal if either both are [Some] and the provided
  assertion passes when applied to their values or if both values are [None].
  Otherwise, the assertion fails.

  @param ctxt The testing context
  @param id A name to be used in error message
  @param assert_equal An assertion on the equality of values
  @param expected The expected value
  @param actual The actual value
  @raise Failure If the values are not equal
  @since 1.0
*)
