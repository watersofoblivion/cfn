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

exception MismatchedTypes of Annot.ty * Loc.t * Annot.ty
(** [MismatchedTypes (inferred, loc, annotated)] is raised when the [inferred]
    type and the [annotated] type at [loc] disagree. *)

exception UnsupportedBinOpPromotion of Loc.t * Syntax.bin * Loc.t * Annot.ty * Loc.t * Annot.ty

(** {2 Desugaring} *)

val desug_ty : Annot.ty Env.t -> Syntax.ty -> (Annot.ty -> 'a) -> 'a
(** [desug_ty env ty kontinue] desugars the syntax type [ty] in the environment
    [env].  The desugared type is passed to the continuation [kontinue]. *)

val desug_un : Annot.ty Env.t -> Syntax.un -> Annot.ty -> (Annot.ty -> Annot.builtin -> 'a) -> 'a
(** [desug_un env op operand kontinue] desugars the unary operator [op]
    operating on a value of type [operand] in the environment [env] into a
    built-in function.  The desugared builtin and its result type passed to the
    continuation [kontinue]. *)

val desug_bin : Annot.ty Env.t -> Syntax.bin -> Annot.ty -> Annot.ty -> (Annot.ty -> Annot.builtin -> 'a) -> 'a
(** [desug_bin env op lhs rhs kontinue] desugars the binary operator [op]
    operating on values of type [lhs] and [rhs] in the environment [env] into a
    built-in function.  The desugared builtin and its result type passed to the
    continuation [kontinue]. *)

val desug_rune : Annot.ty Env.t -> Syntax.rune -> (Uchar.t -> 'a) -> 'a
(** [desug_rune env r kontinue] desugars the syntax rune [r] in the environment
    [env].  The desugared rune is passed to the continuation [kontinue]. *)

val desug_str : Annot.ty Env.t -> Syntax.str list list -> (string -> 'a) -> 'a
(** [desug_str env str kontinue] desugars the syntax string segment [str] in the
    environment [env].  The desugared string segment is passed to the
    continuation [kontinue]. *)

val desug_expr : Annot.ty Env.t -> Syntax.expr -> (Annot.ty -> Annot.expr -> 'a) -> 'a
(** [desug_expr env expr kontinue] desugars the syntax expression [expr] in the
    environment [env].  The desugared expression and its type are passed to the
    continuation [kontinue]. *)

val desug_patt : Annot.ty Env.t -> Syntax.patt -> Annot.ty -> (Annot.ty Env.t -> Annot.patt -> 'a) -> 'a
(** [desug_patt env patt ty kontinue] desugars the syntax pattern [patt] in the
    environment [env] against the type [ty].  The desugared pattern and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)

val desug_binding : Annot.ty Env.t -> Syntax.binding -> (Annot.ty Env.t -> Annot.binding -> 'a) -> 'a
(** [desug_binding env binding kontinue] desugars the syntax binding [binding]
    in the environment [env].  The desugared binding and a (possibly updated)
    environment are passed to the continuation [kontinue]. *)

val desug_top : Annot.ty Env.t -> Syntax.top -> (Annot.ty Env.t -> Annot.top -> 'a) -> 'a
(** [desug_top env top kontinue] desugars the top-level syntax expression [top]
    in the environment [env].  The desugared top-level expression and a
    (possibly updated) environment are passed to the continuation [kontinue]. *)

val desug_file : Annot.ty Env.t -> Syntax.file -> (Annot.ty Env.t -> Annot.top list -> 'a) -> 'a
(** [desug_file env file kontinue] desugars the syntax file [file] in the
    environment [env].  The desugared top-level expressions and a (possibly
    updated) environment are passed to the continuation [kontinue]. *)
