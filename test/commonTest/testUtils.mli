open Format

open OUnit2

(**
 * {1 Testing Utilities}
 *)

val not_equal : ctxt:test_ctxt -> string -> ('a -> formatter -> unit) -> 'a -> 'a -> unit
(** [not_equal ~ctxt msg pp expected actual] fails a test because [expected] was
    not equal to [actual].  [msg] is the type of objects that are not equal and
    should be a plural form (i.e., "Expressions").  [pp] is used to pretty-print
    the two values. *)

val assert_optional_equal : ctxt:test_ctxt -> string -> (ctxt:test_ctxt -> 'a -> 'a -> unit) -> 'a option -> 'a option -> unit
(** [assert_optional_equal ~ctxt id assert_equal expected actual] asserts that
    the values [expected] and [actual] are either both [Some] and are equal
    using [assert_equal], or both [None].  If they are mismatched, an error
    is created using [id] and passed to {!OUnit2.assert_failure}. *)
