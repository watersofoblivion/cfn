(**
  Monomorhpization
*)

open Common

(**
  {1 Exceptions}
*)

exception UnboundIdentifier of {
  id: Sym.t; (** The unbound identifier *)
}
(**
  Raised when an identifier is unbound.

  @since 1.0
*)

exception MismatchedTypes of {
  inferred:  Mono.ty; (** The inferred type *)
  annotated: Mono.ty; (** The annotated type *)
}
(**
  Raised when an inferred type disagrees with a type annotation.

  @since 1.0
*)

exception InvalidArity of {
  expected: int;
  actual:   int;
}
(**
  Raised when a built-in function is applied to an incorrect number of
  arguments.

  @since 1.0
*)

(**
  {1 Monomorhpization}
*)

val mono_ty : Mono.ty Env.t -> Ir.ty -> (Mono.ty -> 'a) -> 'a
(**
  Monomorphize a type.

  @param env The type environment
  @param ty The type to monomorphize
  @param kontinue The continuation the monomorphized type is passed to
  @return The result of the continuation
  @since 1.0
*)

val mono_builtin : Mono.ty Env.t -> Ir.builtin -> (Mono.arity -> Mono.builtin -> 'a) -> 'a
(**
  Monomorphize a built-in function.

  @param env The type environment
  @param builtin The built-in function to monomorphize
  @param kontinue The continuation the monomorphized built-in function and its
    arity are passed to
  @return The result of the continuation
  @since 1.0
*)

val mono_atom : Mono.ty Env.t -> Ir.atom -> (Mono.ty -> Mono.atom -> 'a) -> 'a
(**
  Monomorphize an atomic value.

  @param env The type environment
  @param atom The atomic value to monomorphize
  @param kontinue The contination the monomorphized atom and its type are passed
    to
  @return The result of the continuation
  @since 1.0
*)

val mono_expr : Mono.ty Env.t -> Ir.expr -> (Mono.ty -> Mono.expr -> 'a) -> 'a
(**
  Monomorphize an expression.

  @param env The type environment
  @param expr The expression to monomorphize
  @param kontinue The continuation the monomorphized expression and its type are
    passed to
  @return The result of the continuation
  @since 1.0
*)

val mono_term : Mono.ty Env.t -> Ir.term -> (Mono.ty -> Mono.term -> 'a) -> 'a
(**
  Monomorphize a term.

  @param env The type envionment
  @param term The term to monomorphize
  @param kontinue The continuation the monomorphized term and its type are
    passed to
  @return The result of the continuation
  @since 1.0
*)

val mono_patt : Mono.ty Env.t -> Ir.patt -> Mono.ty -> (Mono.ty Env.t -> Mono.patt -> 'a) -> 'a
(**
  Monomorphize a pattern.

  @param env The type environment
  @param patt The pattern to monomorphize
  @param ty The type of values the pattern must match
  @param kontinue The continuation that the monomorphized pattern and a (possibly
    updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)

val mono_binding : Mono.ty Env.t -> Ir.binding -> (Mono.ty Env.t -> Mono.binding -> 'a) -> 'a
(**
  Monomorphize a value binding.

  @param env The type environment
  @param binding The binding to monomorphize
  @param kontinue The contination the monomorphized binding and a (possibly
    updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)

val mono_top : Mono.ty Env.t -> Ir.top -> (Mono.ty Env.t -> Mono.top -> 'a) -> 'a
(**
  Monomorphize a top-level binding.

  @param env The type environment
  @param binding The binding to monomorphize
  @param kontinue The continuation the monomorphized binding and a (possibly
    updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)
