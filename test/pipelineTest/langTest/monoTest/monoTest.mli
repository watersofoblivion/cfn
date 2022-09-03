(** {1 Monomorphic Syntax Tests} *)

open OUnit2

(** {2 Test Suite} *)

val suite : test
(**
  The unit test suite.

  @since 1.0
*)

(** {2 Assertions} *)

(**
 * {3 Equality}
 *
 * The test context passed in is passed down to all internal assertions.
 *)

val assert_ty_equal : ctxt:test_ctxt -> Mono.ty -> Mono.ty -> unit
(**
  Assert that two types are equal.

  To be considered equal, the types must match on constructors, types of fields
  for complex types, and types of parameters for polymorphic types.

  @param ctxt The testing context
  @param expected The expected type
  @param actual The actual type
  @raise Failure Raised when the types disagree on constructor, field types, or
    parameter types
  @since 1.0
*)

val assert_arity_equal : ctxt:test_ctxt -> Mono.arity -> Mono.arity -> unit
(**
  Assert that two built-in function arities are equal.

  The values are considered equal if they match on the arity kind (fixed vs.
  variable), argument type, and result type.  For fixed arities, argument types
  must match pairwise.

  @param ctxt The testing context
  @param expected The expected arity
  @param actual The actual arity
  @raise Failure Raised when the arities disagree on arity kind, argument
    type(s), or result type
  @since 1.0
*)

val assert_builtin_equal : ctxt:test_ctxt -> Mono.builtin -> Mono.builtin -> unit
(**
  Assert that two built-in functions are equal.

  The values are considered equal if the function names match and any type
  parameters of the functions match pairwise.

  @param ctxt The testing context
  @param expected The expected built-in function
  @param actual The actual built-in function
  @raise Failure Raised when the functions disagree on name or type
  @since 1.0
*)

val assert_atom_equal : ctxt:test_ctxt -> Mono.atom -> Mono.atom -> unit
(**
  Assert that two atomic values are equal.

  The values are considered equal if their kinds (integer literal, identifier,
  etc.) match and all their subcomponents match.

  @param ctxt The testing context
  @param expected The expected atom
  @param actual The actual atom
  @raise Failure Raise when the atomic values disagree on kind or components
  @since 1.0
*)

val assert_expr_equal : ctxt:test_ctxt -> Mono.expr -> Mono.expr -> unit
(**
  Assert that two expressions are equal.

  The values are considered equal if their kinds (built-in function application,
  etc.) match and all their subcomponents match, for example the arguments to
  built-in function applications matching pairwise.

  @param ctxt The testing context
  @param expected The expected expression
  @param actual The actual expression
  @raise Failure Raise when the expressions disagree on kind or components
  @since 1.0
*)

val assert_term_equal : ctxt:test_ctxt -> Mono.term -> Mono.term -> unit
(**
  Assert that two terms are equal.

  The values are considered equal if their kinds (let binding, etc.) match and
  all their subcomponents match, for example the pattern, type, and expression
  of a let binding.

  @param ctxt The testing context
  @param expected The expected term
  @param actual The actual term
  @raise Failure Raise when the terms disagree on kind or components
  @since 1.0
*)

val assert_patt_equal : ctxt:test_ctxt -> Mono.patt -> Mono.patt -> unit
(**
  Assert that two patterns are equal.

  The values are considered equal if their kinds (ground, variable, etc.) match
  and all subcomponents match, for example the symbols of variable patterns.

  @param ctxt The testing context
  @param expected The expected pattern
  @param actual The actual pattern
  @raise Failure Raised when the patterns disagree on kind or components
  @since 1.0
*)

val assert_binding_equal : ctxt:test_ctxt -> Mono.binding -> Mono.binding -> unit
(**
  Assert that two value bindings are equal.

  The values are considered equal if their patterns match, their types match,
  and their expressions match.

  @param ctxt The testing context
  @param expected The expected binding
  @param actual The actual binding
  @raise Failure Raised when the bindings disagree on patterns, types, or
    expressions.
  @since 1.0
*)

val assert_top_equal : ctxt:test_ctxt -> Mono.top -> Mono.top -> unit
(**
  Assert that two top-level bindings are equal.

  The values are considered equal if their bindings match.

  @param ctxt The testing context
  @param expected The expected binding
  @param actual The actual binding
  @raise Failure Raised when the bindings disagree
  @since 1.0
*)
