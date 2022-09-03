(** {1 Closure Conversion} *)

open Common

(** {2 Exceptions} *)

exception UnboundIdentifier of {
  id: Sym.t; (** The unbound identifier *)
}
(**
  Raised when an identifier is unbound.

  @since 1.0
*)

exception MismatchedTypes of {
  inferred:  Clos.ty; (** The inferred type *)
  annotated: Clos.ty; (** The annotated type *)
(**
  Raised when an inferred type and a type annotation disagree.

  @since 1.0
*)

exception InvalidArity of {
  expected: int; (** The expected arity *)
  actual:   int; (** The actual arity *)
}
(**
  Raised when a built-in function is applied to an incorrect number of
  arguments.

  @since 1.0
*)

(** {2 Closure Conversion} *)

val convert_ty : Clos.ty Env.t -> Mono.ty -> (Clos.ty -> 'a) -> 'a
(**
  Closure convert a type.

  @param env The type environment
  @param ty The type to convert
  @param kontinue The continuation the closed type is passed to
  @return The result of the continuation
  @since 1.0
*)

val convert_builtin : Clos.ty Env.t -> Mono.builtin -> (Clos.arity -> Clos.builtin -> 'a) -> 'a
(**
  Closure convert a built-in function.

  @param env The type environment
  @param builtin The built-in function to convert
  @param kontinue The continuation the closed built-in function and its arity
    are passed to
  @return The result of the continuation
  @since 1.0
*)

val convert_atom : Clos.ty Env.t -> Mono.atom -> (Clos.ty -> Clos.atom -> 'a) -> 'a
(**
  Closure convert an atomic value.

  @param env The type environment
  @param atom The atomic value to convert
  @param kontinue The contination the closed atom and its type are passed to
  @return The result of the continuation
  @since 1.0
*)

val convert_expr : Clos.ty Env.t -> Mono.expr -> (Clos.ty -> Clos.expr -> 'a) -> 'a
(**
  Closure convert an expression.

  @param env The type environment
  @param expr The expression to convert
  @param kontinue The continuation the closed expression and its type are
    passed to
  @return The result of the continuation
  @since 1.0
*)

val convert_term : Clos.ty Env.t -> Mono.term -> (Clos.ty -> Clos.term -> 'a) -> 'a
(**
  Closure convert a term.

  @param env The type envionment
  @param term The term to convert
  @param kontinue The continuation the closed term and its type are passed to
  @return The result of the continuation
  @since 1.0
*)

val convert_patt : Clos.ty Env.t -> Mono.patt -> Clos.ty -> (Clos.ty Env.t -> Clos.patt -> 'a) -> 'a
(**
  Closure convert a pattern.

  @param env The type environment
  @param patt The pattern to convert
  @param ty The type of values the pattern must match
  @param kontinue The continuation that the closed pattern and a (possibly
    updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)

val convert_binding : Clos.ty Env.t -> Mono.binding -> (Clos.ty Env.t -> Clos.binding -> 'a) -> 'a
(**
  Closure convert a value binding.

  @param env The type environment
  @param binding The binding to convert
  @param kontinue The contination the closed binding and a (possibly updated)
    type environment are passed to
  @return The result of the continuation
  @since 1.0
*)

val convert_top : Clos.ty Env.t -> Mono.top -> (Clos.ty Env.t -> Clos.top -> 'a) -> 'a
(**
  Closure convert a top-level binding.

  @param env The type environment
  @param binding The binding to convert
  @param kontinue The continuation the closed binding and a (possibly updated)
    type environment are passed to
  @return The result of the continuation
  @since 1.0
*)
