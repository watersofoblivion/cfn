open OUnit2

open Common

(**
 * {1 Symbolization}
 *)

val suite : test
(**
  The unit test suite.

  @since 1.0
*)

(**
 * {2 Helpers}
 *)

val fresh_sym : ?id:string -> unit -> Sym.t
(**
  Generate a fresh identifier from the default sequence.

  @param id If given, passed to {!Sym.gen}
  @return A fresh symbol
  @since 1.0
*)

(**
 * {2 Assertions}
 *)

val assert_sym_equal : ctxt:test_ctxt -> Sym.t -> Sym.t -> unit
(**
  Assert that two symbols are equal.

  The values are considered equal if both symbol index and identifier are equal.

  @param ctxt The testing context
  @param expected The expected symbol value
  @param actual The actual symbol value
  @raise Failure Raised if the symbols disagree on index or identifier
  @since 1.0
*)

val sym_not_equal : ctxt:test_ctxt -> Sym.t -> Sym.t -> unit
(**
  Assert that two symbols are not equal.  The negation of {!assert_sym_equal}.

  @param ctxt The testing context
  @param expected The expected symbol value
  @param actual The actual symbol value
  @raise Failure Raised if the symbols agree on both index and identifier
  @since 1.0
*)
