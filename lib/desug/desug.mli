(** {1 Desugaring} *)

open Common

(** {2 Exceptions} *)

exception InvalidNumberFormat of Loc.t * string * Sym.t * string
(** [InvalidNumberFormat (loc, lexeme, constr, msg)] is raised when the lexeme
    [lexeme] at location [loc] is in a valid format for the type [constr].  The
    [msg] is the error message returned by the conversion function. *)

exception UnboundConstructor of Loc.t * Sym.t
(** [UnboundConstructor (loc, id)] is raised when the type constructor [id] at
    location [loc] is unbound. *)

exception UnboundIdentifier of Loc.t * Sym.t
(** [UnboundIdentifier (loc, id)] is raised when the identifier [id] at location
    [loc] is unbound. *)

exception MismatchedTypes of Annot.Type.t * Loc.t * Annot.Type.t
(** [MismatchedTypes (inferred, loc, annotated)] is raised when the [inferred]
    type and the [annotated] type at [loc] disagree. *)

(** {2 Desugaring} *)

val desug_ty : Annot.Type.t Env.t -> Syntax.Type.t -> (Annot.Type.t -> 'a) -> 'a
(** [desug_ty env ty kontinue] desugars the syntax type [ty] in the environment
    [env].  The desugared type is passed to the continuation [kontinue]. *)

val desug_expr : Annot.Type.t Env.t -> Syntax.Ast.expr -> (Annot.Type.t -> Annot.Ast.expr -> 'a) -> 'a
(** [desug_expr env expr kontinue] desugars the syntax expression [expr] in the
    environment [env].  The desugared expression and its type are passed to the
    continuation [kontinue]. *)

val desug_patt : Annot.Type.t Env.t -> Syntax.Ast.patt -> Annot.Type.t -> (Annot.Type.t Env.t -> Annot.Ast.patt -> 'a) -> 'a
(** [desug_patt env patt ty kontinue] desugars the syntax pattern [patt] in the
    environment [env] against the type [ty].  The desugared pattern and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)

val desug_binding : Annot.Type.t Env.t -> Syntax.Ast.binding -> (Annot.Type.t Env.t -> Annot.Ast.binding -> 'a) -> 'a
(** [desug_binding env binding kontinue] desugars the syntax binding [binding]
    in the environment [env].  The desugared binding and a (possibly updated)
    environment are passed to the continuation [kontinue]. *)

val desug_top : Annot.Type.t Env.t -> Syntax.Ast.top -> (Annot.Type.t Env.t -> Annot.Ast.top -> 'a) -> 'a
(** [desug_top env top kontinue] desugars the top-level syntax expression [top]
    in the environment [env].  The desugared top-level expression and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)

val desug_file : Annot.Type.t Env.t -> Syntax.Ast.file -> (Annot.Type.t Env.t -> Annot.Ast.top list -> 'a) -> 'a
(** [desug_file env file kontinue] desugars the syntax file [file] in the
    environment [env].  The desugared top-level expressions and a (possibly
    updated) environment are passed to the continuation [kontinue]. *)
