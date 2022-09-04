(** {1 Annotated Syntax Tests} *)

open OUnit2

open Common

(** {2 Unit Test Suite} *)

val suite : test
(**
  The unit test suite.

  @since 1.0
*)

(** {2 Fixtures} *)

val fresh_builtin_struct_eq : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh structural equality built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_bool}.
  @return A fresh structural equality built-in function
  @since 1.0
*)

val fresh_builtin_struct_neq : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh structural inequality built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_bool}.
  @return A fresh structural inequality built-in function
  @since 1.0
*)

val fresh_builtin_phys_eq : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh physical equality built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_bool}.
  @return A fresh physical equality built-in function
  @since 1.0
*)

val fresh_builtin_phys_neq : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh physical inequality built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_bool}.
  @return A fresh physical inequality built-in function
  @since 1.0
*)

val fresh_builtin_lt : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh less than built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh less than built-in function
  @since 1.0
*)

val fresh_builtin_lte : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh less than or equal built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh less than or equal built-in function
  @since 1.0
*)

val fresh_builtin_gt : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh greater than built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh greater than built-in function
  @since 1.0
*)

val fresh_builtin_gte : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh greater than or equal built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh greater than or equal built-in function
  @since 1.0
*)

val fresh_builtin_lsl : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh logical shift left built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh logical shift left built-in function
  @since 1.0
*)

val fresh_builtin_lsr : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh logical shift right built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh logical shift right built-in function
  @since 1.0
*)

val fresh_builtin_asl : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh arithmetic shift left built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh arithmetic shift left built-in function
  @since 1.0
*)

val fresh_builtin_asr : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh arithmetic shift right built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh arithmetic shift right built-in function
  @since 1.0
*)

val fresh_builtin_add : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh addition built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh addition built-in function
  @since 1.0
*)

val fresh_builtin_sub : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh subtraction built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh subtraction built-in function
  @since 1.0
*)

val fresh_builtin_mul : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh multiplication built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh multiplication built-in function
  @since 1.0
*)

val fresh_builtin_div : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh division built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh division built-in function
  @since 1.0
*)

val fresh_builtin_mod : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh modulus built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh modulus built-in function
  @since 1.0
*)

val fresh_builtin_exp : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh exponentiation built-in function.

  @param ty The type of the operands.  Defaults to [Float].
  @return A fresh exponentiation built-in function
  @since 1.0
*)

val fresh_builtin_neg : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh negation built-in function.

  @param ty The type of the operand.  Defaults to {!Annot.ty_int}.
  @return A fresh negation built-in function
  @since 1.0
*)

val fresh_builtin_bit_and : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh bitwise AND built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh bitwise AND built-in function
  @since 1.0
*)

val fresh_builtin_bit_or : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh bitwise OR built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh bitwise OR built-in function
  @since 1.0
*)

val fresh_builtin_bit_not : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh bitwise NOT built-in function.

  @param ty The type of the operand.  Defaults to {!Annot.ty_int}.
  @return A fresh bitwise NOT built-in function
  @since 1.0
*)

val fresh_builtin_bit_xor : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh bitwise XOR built-in function.

  @param ty The type of the operands.  Defaults to {!Annot.ty_int}.
  @return A fresh bitwise XOR built-in function
  @since 1.0
*)

val fresh_builtin_log_not : unit -> Annot.builtin
(**
  Construct a fresh logical NOT built-in function.

  @param ty The type of the operand.  Defaults to {!Annot.ty_int}.
  @return A fresh logical NOT built-in function
  @since 1.0
*)

val fresh_builtin_promote : ?sub:Annot.ty -> ?sup:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh type promotion built-in function.

  @param sub The subtype to promote.  Defaults to {!Annot.ty_int}.
  @param sup The supertype to promote to.  Defaults to {!Annot.ty_long}.
  @return A fresh type promotion built-in function
  @since 1.0
*)

val fresh_builtin_concat : ?ty:Annot.ty -> unit -> Annot.builtin
(**
  Construct a fresh concatenation built-in function.

  @param ty The type of the arguments.  Defaults to {!Annot.ty_string}.
  @return A fresh concatenation built-in function
  @since 1.0
*)

val fresh_patt_ground : unit -> Annot.patt
(**
  Construct a fresh ground pattern.

  @return A fresh ground pattern
  @since 1.0
*)

val fresh_patt_var : ?id:Sym.t -> unit -> Annot.patt
(**
  Construct a fresh variable pattern.

  @param id The symbol to use for the identifier.  Defaults to
    {!SymTest.fresh_sym}
  @return A fresh variable pattern
  @since 1.0
*)

(** {2 Assertions} *)

(**
 * {3 Equality}
 *
 * The test context passed in is passed down to all internal assertions.
 *)

val assert_ty_equal : ctxt:test_ctxt -> Annot.ty -> Annot.ty -> unit
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

val assert_arity_equal : ctxt:test_ctxt -> Annot.arity -> Annot.arity -> unit
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

val assert_builtin_equal : ctxt:test_ctxt -> Annot.builtin -> Annot.builtin -> unit
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

val assert_patt_equal : ctxt:test_ctxt -> Annot.patt -> Annot.patt -> unit
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

val assert_expr_equal : ctxt:test_ctxt -> Annot.expr -> Annot.expr -> unit
(**
  Assert that two expressions are equal.

  The values are considered equal if their kinds (integer primitive, built-in
  function application, etc.) match and all their subcomponents match, for
  example the arguments to built-in function applications matching pairwise.

  @param ctxt The testing context
  @param expected The expected expression
  @param actual The actual expression
  @raise Failure Raise when the expressions disagree on kind or components
  @since 1.0
*)

val assert_binding_equal : ctxt:test_ctxt -> Annot.binding -> Annot.binding -> unit
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

val assert_top_equal : ctxt:test_ctxt -> Annot.top -> Annot.top -> unit
(**
  Assert that two top-level bindings are equal.

  The values are considered equal if their bindings match.

  @param ctxt The testing context
  @param expected The expected binding
  @param actual The actual binding
  @raise Failure Raised when the bindings disagree
  @since 1.0
*)
