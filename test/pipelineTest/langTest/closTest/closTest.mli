(** {1 Closure Converted Syntax Tests} *)

 open OUnit2

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

val assert_ty_equal : ctxt:test_ctxt -> Clos.ty -> Clos.ty -> unit
(** [assert_ty_equal ~ctxt expected actual] asserts that the type [actual] is
    equal to the type [expected]. *)

val assert_builtin_equal : ctxt:test_ctxt -> Clos.builtin -> Clos.builtin -> unit
(** [assert_builtin_equal ~ctxt expected actual] asserts that the built-in
    function [actual] is equal to the built-in function [expected]. *)

val assert_atom_equal : ctxt:test_ctxt -> Clos.atom -> Clos.atom -> unit
(** [assert_atom_equal ~ctxt expected actual] asserts that the atomic value
    [actual] is equal to the atomic value [expected]. *)

val assert_expr_equal : ctxt:test_ctxt -> Clos.expr -> Clos.expr -> unit
(** [assert_expr_equal ~ctxt expected actual] asserts that the expression
    [actual] is equal to the expression [expected]. *)

val assert_block_equal : ctxt:test_ctxt -> Clos.block -> Clos.block -> unit
(** [assert_block_equal ~ctxt expected actual] asserts that the block [actual]
    is equal to the block [expected]. *)

val assert_patt_equal : ctxt:test_ctxt -> Clos.patt -> Clos.patt -> unit
(** [assert_patt_equal ~ctxt expected actual] asserts that the pattern [actual]
    is equal to the pattern [expected]. *)

val assert_binding_equal : ctxt:test_ctxt -> Clos.binding -> Clos.binding -> unit
(** [assert_binding_equal ~ctxt expected actual] asserts that the binding
    [actual] is equal to the binding [expected]. *)

val assert_top_equal : ctxt:test_ctxt -> Clos.top -> Clos.top -> unit
(** [assert_top_equal ~ctxt expected actual] asserts that the top-level
    expression [actual] is equal to the top-level expression [expected]. *)
