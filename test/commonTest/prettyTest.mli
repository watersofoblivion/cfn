(**
  Pretty Printing
*)

open Format

open OUnit2

(**
  {1 Test Suite}
*)

val suite : test
(**
  The unit test suite.

  @since 1.0
*)

(**
  {1 Assertions}
*)

val assert_pp : ctxt:test_ctxt -> (formatter -> 'a -> unit) -> string list -> 'a -> unit
(**
  Assert that a pretty-printer produces specific output when applied to a value.

  The output is considered equal if the output is byte-for-byte equal to the
  input lines joined with newlines.  Notably, leading and trailing whitespace on
  either the expected and actual output is {i not} trimmed.

  @param ctxt The testing context
  @param pp The pretty-printer
  @param output The expected output, as a list of lines
  @param value The value to pretty-print
  @raise Failue Raised when the pretty printer does not produce the expected
    output
*)
