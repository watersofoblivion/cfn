(** {1 Monomorhpization} *)

open Common

(** {2 Exceptions} *)

exception UnboundIdentifier of Sym.t
(** [UnboundIdentifier id] is raised when the identifier [id] is unbound. *)

exception MismatchedTypes of Mono.ty * Mono.ty
(** [MismatchedTypes (inferred, annotated)] is raised when the [inferred] type
    and the [annotated] types disagree. *)

(** {2 Monomorhpization} *)

val convert_ty : Mono.ty Env.t -> Ir.ty -> (Mono.ty -> 'a) -> 'a
(** [convert_ty env ty kontinue] monomorphizes the ANF type [ty] in the
    environment [env].  The monomorphic type is passed to the continuation
    [kontinue]. *)

val convert_atom : Mono.ty Env.t -> Ir.atom -> (Mono.ty -> Mono.atom -> 'a) -> 'a
(** [convert_atom env atom kontinue] monomorphizes the ANF atomic value [atom]
    in the environment [env].  The monomorphic atomic value and its type are
    passed to the continuation [kontinue]. *)

val convert_expr : Mono.ty Env.t -> Ir.expr -> (Mono.ty -> Mono.expr -> 'a) -> 'a
(** [convert_expr env expr kontinue] monomorphizes the ANF exprression [expr] in
    the environment [env].  The monomorphic expression and its type are passed
    to the continuation [kontinue]. *)

val convert_block : Mono.ty Env.t -> Ir.block -> (Mono.ty -> Mono.block -> 'a) -> 'a
(** [convert_block env block kontinue] monomorphizes the ANF block [block] in
    the environment [env].  The monomorphic block and its type are passed to the
    continuation [kontinue]. *)

val convert_patt : Mono.ty Env.t -> Ir.patt -> Mono.ty -> (Mono.ty Env.t -> Mono.patt -> 'a) -> 'a
(** [convert_patt env patt ty kontinue] monomorphizes the ANF pattern [patt] in
    the environment [env] against the type [ty].  The monomorphic pattern and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)

val convert_binding : Mono.ty Env.t -> Ir.binding -> (Mono.ty Env.t -> Mono.binding -> 'a) -> 'a
(** [convert_binding env binding kontinue] monomorphizes the ANF binding
    [binding] in the environment [env].  The monomorphic binding and a (possibly
    updated) environment are passed to the continuation [kontinue]. *)

val convert_top : Mono.ty Env.t -> Ir.top -> (Mono.ty Env.t -> Mono.top -> 'a) -> 'a
(** [convert_top env top kontinue] monomorphizes the ANF top-level expression
    [top] in the environment [env].  The monomorphic top-level expression and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)
