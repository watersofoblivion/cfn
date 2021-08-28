(** {1 A-Normalization} *)

open Common

(** {2 Exceptions} *)

exception UnboundIdentifier of Sym.t
(** [UnboundIdentifier id] is raised when the identifier [id] is unbound. *)

exception MismatchedTypes of Ir.Type.t * Ir.Type.t
(** [MismatchedTypes (inferred, annotated)] is raised when the [inferred] type
    and the [annotated] types disagree. *)

(** {2 Normalization} *)

val norm_ty : Ir.Type.t Env.t -> Annot.Type.t -> (Ir.Type.t -> 'a) -> 'a
(** [norm_ty env ty kontinue] normalizes the annotated type [ty] in the
    environment [env].  The normalized type is passed to the continutation
    [kontinue]. *)

val norm_expr : Ir.Type.t Env.t -> Annot.Ast.expr -> (Ir.Type.t -> Ir.Ast.block -> 'a) -> 'a
(** [norm_expr env expr kontinue] normalizes the annotated expression [expr] in
    the environment [env].  The normalized block and its type are passed to the
    continuation [kontinue]. *)

val norm_patt : Ir.Type.t Env.t -> Annot.Ast.patt -> Ir.Type.t -> (Ir.Type.t Env.t -> Ir.Ast.patt -> 'a) -> 'a
(** [norm_patt env patt ty kontinue] normalizes the annotated pattern [patt] in
    the environment [env] against the type [ty].  The normalized pattern and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)

val norm_binding : Ir.Type.t Env.t -> Annot.Ast.binding -> (Ir.Type.t Env.t -> Ir.Ast.binding -> 'a) -> 'a
(** [norm_binding env binding kontinue] normalizes the binding [binding] in the
    environment [env].  The normalized binding and a (possibly updated)
    environment are passed to the continuation [kontinue]. *)

val norm_top : Ir.Type.t Env.t -> Annot.Ast.top -> (Ir.Type.t Env.t -> Ir.Ast.top -> 'a) -> 'a
(** [norm_top env top kontinue] normalizes the top-level expression [top] in the
    environment [env].  The normalized top-level expression and a (possibly
    updated) environment are passed to the continuation [kontinue]. *)
