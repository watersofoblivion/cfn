(**
 * {1 Abstract Syntax}
 *)

 open OUnit2

 open Clos

(** {2 Test Suite} *)

val suite : test
(** [suite] is the unit test suite. *)

(** {2 Assertions} *)

(**
 * {3 Equality}
 *
 * All equality assertions include location equality.  To normalize location
 * information, use the [deloc_...] family of functions.
 *
 * The test context passed in is passed down to all internal assertions.
 *)

val assert_atom_equal : ctxt:test_ctxt -> Ast.atom -> Ast.atom -> unit
(** [assert_atom_equal ~ctxt expected actual] asserts that the atomic value
    [actual] is equal to the atomic value [expected]. *)

val assert_expr_equal : ctxt:test_ctxt -> Ast.expr -> Ast.expr -> unit
(** [assert_expr_equal ~ctxt expected actual] asserts that the expression
    [actual] is equal to the expression [expected]. *)

val assert_block_equal : ctxt:test_ctxt -> Ast.block -> Ast.block -> unit
(** [assert_block_equal ~ctxt expected actual] asserts that the block [actual]
    is equal to the block [expected]. *)
