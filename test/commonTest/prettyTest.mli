open Format

open OUnit2

(**
 * {1 Pretty Printing}
 *)

val suite : test
(** [suite] is the test suite *)

(**
 * {2 Assertions}
 *)

val assert_pp : ctxt:test_ctxt -> (formatter -> 'a -> unit) -> string list -> 'a -> unit
(** [assert_pp ~ctxt pp output value] asserts that the pretty-printer [pp]
    applied to the value [value] produces the output [output] joined by
    newlines.  The testing context [ctxt] is passed to all assertions. *)
