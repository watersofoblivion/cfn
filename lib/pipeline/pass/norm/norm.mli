(** {1 A-Normalization} *)

open Common

(** {2 Exceptions} *)

exception UnboundIdentifier of {
  id: Sym.t; (* The unbound identifier *)
}
(**
  Raised when an identifier is unbound.

  @since 1.0
*)

exception MismatchedTypes of {
  inferred:  Ir.ty; (** The inferred type *)
  annotated: Ir.ty; (** The annotated type *)
}
(**
  Raised when an inferred type disagrees with a type annotation.

  @since 1.0
*)

(** {2 Normalization} *)

val norm_ty : Ir.ty Env.t -> Annot.ty -> (Ir.ty -> 'a) -> 'a
(**
  Normalize a type.

  @param env The type environment
  @param ty The type to normalize
  @param kontinue The continuation that is passed the normalized type
  @return The result of the continuation
  @since 1.0
*)

val norm_builtin : Ir.ty Env.t -> Annot.builtin -> (Ir.arity -> Ir.builtin -> 'a) -> 'a
(**
  Normalize a built-in function.

  @param env The type environment
  @param builtin The built-in function to normalize
  @param kontinue The continuation that the normalized built-in function and its
    arity are passed to
  @return The result of the continuation
  @since 1.0
*)

val norm_expr : Ir.ty Env.t -> Annot.expr -> (Ir.ty -> Ir.term -> 'a) -> 'a
(**
  Noramlize an expression into a term.

  @param env The type environment
  @param expr The expression to normalize
  @param kontinue The continuation the normalized term and its type are passed
    to
  @return The result of the continuation
  @since 1.0
*)

val norm_patt : Ir.ty Env.t -> Annot.patt -> Ir.ty -> (Ir.ty Env.t -> Ir.patt -> 'a) -> 'a
(**
  Normalize a pattern.

  @param env The type environment
  @param patt The pattern to normalize
  @param ty The type of values the pattern must match
  @param kontinue The continuation that the normalized pattern and a (possibly
    updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)

val norm_binding : Ir.ty Env.t -> Annot.binding -> (Ir.ty Env.t -> Ir.binding -> 'a) -> 'a
(**
  Normalize a value binding.

  @param env The type environment
  @param binding The binding to normalize
  @param kontinue The continuation the normalized binding and a (possibly
    updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)

val norm_top : Ir.ty Env.t -> Annot.top -> (Ir.ty Env.t -> Ir.top -> 'a) -> 'a
(**
  Normalize a top-level binding.

  @param env The type environment
  @param top The top-level binding to normalize
  @param kontinue The continuation the normalized top-level binding and a
    (possibly updated) type envionment are passed to
  @since 1.0
*)
