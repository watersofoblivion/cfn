(** {1 Closure Conversion} *)

open Common

(** {2 Exceptions} *)

exception UnboundIdentifier of Sym.t
(** [UnboundIdentifier id] is raised when the identifier [id] is unbound. *)

exception MismatchedTypes of Clos.Type.t * Clos.Type.t
(** [MismatchedTypes (inferred, annotated)] is raised when the [inferred] type
    and the [annotated] types disagree. *)

(** {2 Closure Conversion} *)

val convert_ty : Clos.Type.t Env.t -> Mono.Type.t -> (Clos.Type.t -> 'a) -> 'a
(** [convert_ty env ty kontinue] closure-converts the monomorphic type [ty] in
    the environment [env].  The closure-converted type is passed to the
    continuation [kontinue]. *)

val convert_atom : Clos.Type.t Env.t -> Mono.Ast.atom -> (Clos.Type.t -> Clos.Ast.atom -> 'a) -> 'a
(** [convert_atom env atom kontinue] closure-converts the monomorphic atomic
    value [atom] in the environment [env].  The closure-converted atomic value
    and its type are passed to the continuation [kontinue]. *)

val convert_expr : Clos.Type.t Env.t -> Mono.Ast.expr -> (Clos.Type.t -> Clos.Ast.expr -> 'a) -> 'a
(** [convert_expr env expr kontinue] closure-converts the monomorphic
    exprression [expr] in the environment [env].  The closure-converted
    expression and its type are passed to the continuation [kontinue]. *)

val convert_block : Clos.Type.t Env.t -> Mono.Ast.block -> (Clos.Type.t -> Clos.Ast.block -> 'a) -> 'a
(** [convert_block env block kontinue] closure-converts the monomorphic block
    [block] in the environment [env].  The closure-converted block and its type
    are passed to the continuation [kontinue]. *)

val convert_patt : Clos.Type.t Env.t -> Mono.Ast.patt -> Clos.Type.t -> (Clos.Type.t Env.t -> Clos.Ast.patt -> 'a) -> 'a
(** [convert_patt env patt kontinue] closure-converts the monomorphic pattern
    [patt] in the environment [env] against the type [ty].  The
    closure-converted pattern and a (possibly updated) environment are passed to
    the continuation [kontinue]. *)

val convert_binding : Clos.Type.t Env.t -> Mono.Ast.binding -> (Clos.Type.t Env.t -> Clos.Ast.binding -> 'a) -> 'a
(** [convert_binding env binding kontinue] closure-converts the monomorphic
    binding [binding] in the environment [env].  The closure-converted binding
    and a (possibly updated) environment are passed to the continuation
    [kontinue]. *)

val convert_top : Clos.Type.t Env.t -> Mono.Ast.top -> (Clos.Type.t Env.t -> Clos.Ast.top -> 'a) -> 'a
(** [convert_top env top kontinue] closure-converts the monomorphic top-level
    expression [top] in the environment [env].  The closure-converted top-level
    expression and a (possibly updated) environment are passed to the
    continuation [kontinue]. *)
