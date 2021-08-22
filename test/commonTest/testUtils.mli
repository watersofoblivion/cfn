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
