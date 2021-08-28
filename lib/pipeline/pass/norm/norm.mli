(** {1 A-Normalization} *)

open Common

(** {2 Exceptions} *)

exception UnboundIdentifier of Sym.t
(** [UnboundIdentifier id] is raised when the identifier [id] is unbound. *)

exception MismatchedTypes of Ir.ty * Ir.ty
(** [MismatchedTypes (inferred, annotated)] is raised when the [inferred] type
    and the [annotated] types disagree. *)

(** {2 Normalization} *)

val norm_ty : Ir.ty Env.t -> Annot.ty -> (Ir.ty -> 'a) -> 'a
(** [norm_ty env ty kontinue] normalizes the annotated type [ty] in the
    environment [env].  The normalized type is passed to the continutation
    [kontinue]. *)

val norm_expr : Ir.ty Env.t -> Annot.expr -> (Ir.ty -> Ir.block -> 'a) -> 'a
(** [norm_expr env expr kontinue] normalizes the annotated expression [expr] in
    the environment [env].  The normalized block and its type are passed to the
    continuation [kontinue]. *)

val norm_patt : Ir.ty Env.t -> Annot.patt -> Ir.ty -> (Ir.ty Env.t -> Ir.patt -> 'a) -> 'a
(** [norm_patt env patt ty kontinue] normalizes the annotated pattern [patt] in
    the environment [env] against the type [ty].  The normalized pattern and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)

val norm_binding : Ir.ty Env.t -> Annot.binding -> (Ir.ty Env.t -> Ir.binding -> 'a) -> 'a
(** [norm_binding env binding kontinue] normalizes the binding [binding] in the
    environment [env].  The normalized binding and a (possibly updated)
    environment are passed to the continuation [kontinue]. *)

val norm_top : Ir.ty Env.t -> Annot.top -> (Ir.ty Env.t -> Ir.top -> 'a) -> 'a
(** [norm_top env top kontinue] normalizes the top-level expression [top] in the
    environment [env].  The normalized top-level expression and a (possibly
    updated) environment are passed to the continuation [kontinue]. *)
