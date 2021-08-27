(** {1 Closure Conversion} *)

open Common

(** {2 Closure Conversion} *)

val convert_ty : Mono.Type.t Env.t -> Mono.Type.t -> (Clos.Type.t -> 'a) -> 'a
(** [convert_ty env ty kontinue] closure-converts the monomorphic type [ty] in
    the environment [env].  The closure-converted type is passed to the
    continuation [kontinue]. *)

val convert_atom : Mono.Type.t Env.t -> Mono.Ast.atom -> (Clos.Ast.atom -> 'a) -> 'a
(** [convert_atom env atom kontinue] closure-converts the monomorphic atomic
    value [atom] in the environment [env].  The closure-converted atomic value
    is passed to the continuation [kontinue]. *)

val convert_expr : Mono.Type.t Env.t -> Mono.Ast.expr -> (Clos.Ast.expr -> 'a) -> 'a
(** [convert_expr env expr kontinue] closure-converts the monomorphic
    exprression [expr] in the environment [env].  The closure-converted
    expression is passed to the continuation [kontinue]. *)

val convert_block : Mono.Type.t Env.t -> Mono.Ast.block -> (Clos.Ast.block -> 'a) -> 'a
(** [convert_block env block kontinue] closure-converts the monomorphic block
    [block] in the environment [env].  The closure-converted block is passed to
    the continuation [kontinue]. *)

val convert_patt : Mono.Type.t Env.t -> Mono.Ast.patt -> (Clos.Ast.patt -> 'a) -> 'a
(** [convert_patt env patt kontinue] closure-converts the monomorphic pattern
    [patt] in the environment [env].  The closure-converted pattern is passed to
    the continuation [kontinue]. *)

val convert_binding : Mono.Type.t Env.t -> Mono.Ast.binding -> (Clos.Ast.binding -> 'a) -> 'a
(** [convert_binding env binding kontinue] closure-converts the monomorphic
    binding [binding] in the environment [env].  The closure-converted binding
    is passed to the continuation [kontinue]. *)

val convert_top : Mono.Type.t Env.t -> Mono.Ast.top -> (Clos.Ast.top -> 'a) -> 'a
(** [convert_top env top kontinue] closure-converts the monomorphic top-level
    expression [top] in the environment [env].  The closure-converted top-level
    expression is passed to the continuation [kontinue]. *)
