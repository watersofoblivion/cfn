(** {1 Type Checking} *)

open Common

(** {2 Exceptions} *)

exception UnboundIdentifier of Sym.t
(** [UnboundIdentifier id] is raised when the identifier [id] is unbound. *)

exception MismatchedTypes of Type.t * Type.t
(** [MismatchedTypes (inferred, annotated)] is raised when the [inferred] type
    and the [annotated] type disagree. *)

(** {2 Type Checker} *)

val check_expr : Type.t Env.t -> Ast.expr -> (Type.t -> 'a) -> 'a
(** [check_expr env expr kontinue] type-checks the expression [expr] in the
    environment [env].  The type of the expression is passed to the continuation
    [kontinue]. *)

val check_patt : Type.t Env.t -> Ast.patt -> Type.t -> (Type.t Env.t -> 'a) -> 'a
(** [check_patt env patt ty kontinue] type-checks the pattern [patt] against the
    type [ty] in the environment [env].  A (possibly updated) environment is
    passed to the continuation [kontinue]. *)

val check_binding : Type.t Env.t -> Ast.binding -> (Type.t Env.t -> 'a) -> 'a
(** [check_binding env binding kontinue] type-checks the binding [binding] in
    the environment [env].  A (possibly updated) environment is passed to the
    continuation [kontinue]. *)

val check_top : Type.t Env.t -> Ast.top -> (Type.t Env.t -> 'a) -> 'a
(** [check_top env top kontinue] type-checks the top-level expression [top] in
    the environment [env].  A (possibly updated) environment is passed to the
    continuation [kontinue]. *)
