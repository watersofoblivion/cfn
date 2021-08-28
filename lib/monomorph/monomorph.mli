(** {1 Monomorhpization} *)

open Common

(** {2 Exceptions} *)

exception UnboundIdentifier of Sym.t
(** [UnboundIdentifier id] is raised when the identifier [id] is unbound. *)

exception MismatchedTypes of Mono.Type.t * Mono.Type.t
(** [MismatchedTypes (inferred, annotated)] is raised when the [inferred] type
    and the [annotated] types disagree. *)

(** {2 Monomorhpization} *)

val convert_ty : Mono.Type.t Env.t -> Ir.Type.t -> (Mono.Type.t -> 'a) -> 'a
(** [convert_ty env ty kontinue] monomorphizes the ANF type [ty] in the
    environment [env].  The monomorphic type is passed to the continuation
    [kontinue]. *)

val convert_atom : Mono.Type.t Env.t -> Ir.Ast.atom -> (Mono.Type.t -> Mono.Ast.atom -> 'a) -> 'a
(** [convert_atom env atom kontinue] monomorphizes the ANF atomic value [atom]
    in the environment [env].  The monomorphic atomic value and its type are
    passed to the continuation [kontinue]. *)

val convert_expr : Mono.Type.t Env.t -> Ir.Ast.expr -> (Mono.Type.t -> Mono.Ast.expr -> 'a) -> 'a
(** [convert_expr env expr kontinue] monomorphizes the ANF exprression [expr] in
    the environment [env].  The monomorphic expression and its type are passed
    to the continuation [kontinue]. *)

val convert_block : Mono.Type.t Env.t -> Ir.Ast.block -> (Mono.Type.t -> Mono.Ast.block -> 'a) -> 'a
(** [convert_block env block kontinue] monomorphizes the ANF block [block] in
    the environment [env].  The monomorphic block and its type are passed to the
    continuation [kontinue]. *)

val convert_patt : Mono.Type.t Env.t -> Ir.Ast.patt -> Mono.Type.t -> (Mono.Type.t Env.t -> Mono.Ast.patt -> 'a) -> 'a
(** [convert_patt env patt ty kontinue] monomorphizes the ANF pattern [patt] in
    the environment [env] against the type [ty].  The monomorphic pattern and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)

val convert_binding : Mono.Type.t Env.t -> Ir.Ast.binding -> (Mono.Type.t Env.t -> Mono.Ast.binding -> 'a) -> 'a
(** [convert_binding env binding kontinue] monomorphizes the ANF binding
    [binding] in the environment [env].  The monomorphic binding and a (possibly
    updated) environment are passed to the continuation [kontinue]. *)

val convert_top : Mono.Type.t Env.t -> Ir.Ast.top -> (Mono.Type.t Env.t -> Mono.Ast.top -> 'a) -> 'a
(** [convert_top env top kontinue] monomorphizes the ANF top-level expression
    [top] in the environment [env].  The monomorphic top-level expression and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)
