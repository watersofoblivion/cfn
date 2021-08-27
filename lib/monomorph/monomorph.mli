(** {1 Monomorhpization} *)

open Common

(** {2 Monomorhpization} *)

val convert_ty : Ir.Type.t Env.t -> Ir.Type.t -> (Mono.Type.t -> 'a) -> 'a
(** [convert_ty env ty kontinue] monomorphizes the ANF type [ty] in the
    environment [env].  The monomorphic type is passed to the continuation
    [kontinue]. *)

val convert_atom : Ir.Type.t Env.t -> Ir.Ast.atom -> (Mono.Ast.atom -> 'a) -> 'a
(** [convert_atom env atom kontinue] monomorphizes the ANF atomic value [atom]
    in the environment [env].  The monomorphic atomic value is passed to the
    continuation [kontinue]. *)

val convert_expr : Ir.Type.t Env.t -> Ir.Ast.expr -> (Mono.Ast.expr -> 'a) -> 'a
(** [convert_expr env expr kontinue] monomorphizes the ANF exprression [expr] in
    the environment [env].  The monomorphic expression is passed to the
    continuation [kontinue]. *)

val convert_block : Ir.Type.t Env.t -> Ir.Ast.block -> (Mono.Ast.block -> 'a) -> 'a
(** [convert_block env block kontinue] monomorphizes the ANF block [block] in
    the environment [env].  The monomorphic block is passed to the continuation
    [kontinue]. *)

val convert_patt : Ir.Type.t Env.t -> Ir.Ast.patt -> (Mono.Ast.patt -> 'a) -> 'a
(** [convert_patt env patt kontinue] monomorphizes the ANF pattern [patt] in the
    environment [env].  The monomorphic pattern is passed to the continuation
    [kontinue]. *)

val convert_binding : Ir.Type.t Env.t -> Ir.Ast.binding -> (Mono.Ast.binding -> 'a) -> 'a
(** [convert_binding env binding kontinue] monomorphizes the ANF binding
    [binding] in the environment [env].  The monomorphic binding is passed to
    the continuation [kontinue]. *)

val convert_top : Ir.Type.t Env.t -> Ir.Ast.top -> (Mono.Ast.top -> 'a) -> 'a
(** [convert_top env top kontinue] monomorphizes the ANF top-level expression
    [top] in the environment [env].  The monomorphic top-level expression is
    passed to the continuation [kontinue]. *)
