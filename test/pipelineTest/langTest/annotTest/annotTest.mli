(** {1 Annotated Syntax Tests} *)

open OUnit2

open Common

(** {2 Test Suite} *)

val suite : test
(** [suite] is the unit test suite. *)

(** {2 Fixtures} *)

val fresh_builtin_struct_eq : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_struct_eq ?ty ()] constructs a fresh built-in structural
    equality function operating on values of type [ty]. *)

val fresh_builtin_struct_neq : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_struct_neq ?ty ()] constructs a fresh built-in structural
    inequality function operating on values of type [ty]. *)

val fresh_builtin_phys_eq : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_phys_eq ?ty ()] constructs a fresh built-in physical equality
    function operating on values of type [ty]. *)

val fresh_builtin_phys_neq : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_phys_neq ?ty ()] constructs a fresh built-in physical
    inequality function operating on values of type [ty]. *)

val fresh_builtin_lt : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_lt ?ty ()] constructs a fresh built-in less than function
    operating on values of type [ty]. *)

val fresh_builtin_lte : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_lte ?ty ()] constructs a fresh built-in less than or equal
    function operating on values of type [ty]. *)

val fresh_builtin_gt : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_gt ?ty ()] constructs a fresh built-in greater than function
    operating on values of type [ty]. *)

val fresh_builtin_gte : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_gte ?ty ()] constructs a fresh built-in greater than or equal
    function operating on values of type [ty]. *)

val fresh_builtin_lsl : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_lsl ?ty ()] constructs a fresh built-in logical shift left
    function operating on values of type [ty]. *)

val fresh_builtin_lsr : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_lsr ?ty ()] constructs a fresh built-in logical shift right
    function operating on values of type [ty]. *)

val fresh_builtin_asl : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_asl ?ty ()] constructs a fresh built-in Arithmetic shift left
    function operating on values of type [ty]. *)

val fresh_builtin_asr : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_asr ?ty ()] constructs a fresh built-in Arithmetic shift
    right function operating on values of type [ty]. *)

val fresh_builtin_add : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_add ?ty ()] constructs a fresh built-in addition function
    operating on values of type [ty]. *)

val fresh_builtin_sub : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_sub ?ty ()] constructs a fresh built-in subtraction function
    operating on values of type [ty]. *)

val fresh_builtin_mul : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_mul ?ty ()] constructs a fresh built-in multiplication
    function operating on values of type [ty]. *)

val fresh_builtin_div : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_div ?ty ()] constructs a fresh built-in division function
    operating on values of type [ty]. *)

val fresh_builtin_mod : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_mod ?ty ()] constructs a fresh built-in modulus function
    operating on values of type [ty]. *)

val fresh_builtin_exp : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_exp ?ty ()] constructs a fresh built-in exponentiation
    function operating on values of type [ty]. *)

val fresh_builtin_neg : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_neg ?ty ()] constructs a fresh built-in negation function
    operating on values of type [ty]. *)

val fresh_builtin_bit_and : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_bit_and ?ty ()] constructs a fresh built-in bitwise AND
    function operating on values of type [ty]. *)

val fresh_builtin_bit_or : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_bit_or ?ty ()] constructs a fresh built-in bitwise OR
    function operating on values of type [ty]. *)

val fresh_builtin_bit_not : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_bit_not ?ty ()] constructs a fresh built-in bitwise NOT
    function operating on values of type [ty]. *)

val fresh_builtin_bit_xor : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_bit_xor ?ty ()] constructs a fresh built-in bitwise XOR
    function operating on values of type [ty]. *)

val fresh_builtin_log_not : unit -> Annot.builtin
(** [fresh_builtin_log_not ?ty ()] constructs a fresh built-in logical NOT
    function operating on values of type [ty]. *)

val fresh_builtin_promote : ?sub:Annot.ty -> ?sup:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_promote ?sub ?sup ()] constructs a fresh built-in type
    promotion function promoting values of type [sup] to values of type [sup]. *)

val fresh_builtin_concat : ?ty:Annot.ty -> unit -> Annot.builtin
(** [fresh_builtin_concat ?ty ()] constructs a fresh built-in concatenation
    function operating on values of type [ty]. *)

val fresh_patt_ground : unit -> Annot.patt
(** [fresh_patt_ground ()] constructs a fresh ground pattern. *)

val fresh_patt_var : ?id:Sym.t -> unit -> Annot.patt
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
