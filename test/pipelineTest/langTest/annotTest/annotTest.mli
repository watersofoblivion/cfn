(** {1 Annotated Syntax Tests} *)

open OUnit2

open Common

(** {2 Test Suite} *)

val suite : test
(** [suite] is the unit test suite. *)

(** {2 Fixtures} *)

val fresh_builtin : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin ?ty ()] constructs a fresh built-in function operating on
    values of type [ty]. *)

val fresh_patt_ground : unit -> Annot.patt
(** [fresh_patt_ground ()] constructs a fresh ground pattern. *)

val fresh_patt_var : ?seq:Sym.seq -> ?id:string -> unit -> Annot.patt
(** [fresh_patt_var ?seq ?id ()] constructs a fresh variable pattern. *)

(** {2 Assertions} *)

(**
 * {3 Equality}
 *
 * All equality assertions include location equality.  To normalize location
 * information, use the [deloc_...] family of functions.
 *
 * The test context passed in is passed down to all internal assertions.
 *)

val assert_ty_equal : ctxt:test_ctxt -> Annot.ty -> Annot.ty -> unit
(** [assert_ty_equal ~ctxt expected actual] asserts that the type [actual] is
    equal to the type [expected]. *)

val assert_arity_equal : ctxt:test_ctxt -> Annot.arity -> Annot.arity -> unit
(** [assert_arity_equal ~ctxt expected actual] asserts that the built-in
    function arity [actual] is equal to the built-in function arity [expected]. *)

val assert_builtin_equal : ctxt:test_ctxt -> Annot.builtin -> Annot.builtin -> unit
(** [assert_builtin_equal ~ctxt expected actual] asserts that the built-in
    function [actual] is equal to the built-in function [expected]. *)

val assert_patt_equal : ctxt:test_ctxt -> Annot.patt -> Annot.patt -> unit
(** [assert_patt_equal ~ctxt expected actual] asserts that the pattern [actual]
    is equal to the pattern [expected]. *)

val assert_expr_equal : ctxt:test_ctxt -> Annot.expr -> Annot.expr -> unit
(** [assert_expr_equal ~ctxt expected actual] asserts that the expression
    [actual] is equal to the expression [expected]. *)

val assert_binding_equal : ctxt:test_ctxt -> Annot.binding -> Annot.binding -> unit
(** [assert_binding_equal ~ctxt expected actual] asserts that the binding
    [actual] is equal to the binding [expected]. *)

val assert_top_equal : ctxt:test_ctxt -> Annot.top -> Annot.top -> unit
(** [assert_top_equal ~ctxt expected actual] asserts that the top-level
    expression [actual] is equal to the top-level expression [expected]. *)
