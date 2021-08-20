open OUnit2

open Common

(**
 * {1 Symbolization}
 *)

val suite : test
(** [suite] is the test suite *)

(**
 * {2 Helpers}
 *)

val fresh_sym : ?id:string -> unit -> Sym.t
(** [fresh_sym ?id _] generates a fresh identifier from the default sequence.
    If [id] is given, it is passed to {!Sym.gen}. *)

(**
 * {2 Assertions}
 *)

val assert_sym_equal : ctxt:test_ctxt -> Sym.t -> Sym.t -> unit
(** [assert_sym_equal ~ctxt expected actual] asserts that the symbol [expected]
    is equal to the symbol [actual].  Both symbol index and identifier are
    compared.  The [ctxt] is passed to all internal assertions. *)

val sym_not_equal : ctxt:test_ctxt -> Sym.t -> Sym.t -> unit
(** [sym_not_equal ~ctxt expected actual] asserts that the symbol
    [expected] is not equal to the symbol [actual].  Both symbol index and
    identifier are compared.  The [ctxt] is passed to all internal assertions. *)
