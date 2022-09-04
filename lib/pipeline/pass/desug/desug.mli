(** {1 Desugaring} *)

open Common

(** {2 Exceptions} *)

exception InvalidNumberFormat of {
  loc:    Loc.t;  (** Location of the lexeme *)
  lexeme: string; (** Lexeme *)
  constr: Sym.t;  (** Type constructor *)
  msg:    string  (** Conversion error *)
}
(**
  Raised when a lexeme cannot be parsed as a valid number for a given type
  constructor.

  @since 1.0
*)

exception UnboundConstructor of {
  loc: Loc.t; (** Location of the constructor *)
  id:  Sym.t; (** Unknown constructor *)
}
(**
  Raised when a type constructor is unbound.

  @since 1.0
*)

exception UnboundIdentifier of {
  loc: Loc.t;
  id:  Sym.t;
}
(**
  Raised when an identifier is unbound.

  @since 1.0
*)

exception MismatchedTypes of {
  loc:       Loc.t;    (** Location of the expression *)
  inferred:  Annot.ty; (** Inferred type *)
  annotated: Annot.ty; (** Annotated type *)
}
(**
  Raised when an inferred disagrees with a type annotation.

  @since 1.0
*)

exception UnsupportedBinOpPromotion of {
  loc: Loc.t;      (** Location of the promotion *)
  op:  Syntax.bin; (** The binary operator *)
  sub: Annot.ty;   (** The unpromotable subtype *)
  sup: Annot.ty;   (** The target supertype *)
}
(**
  Raised when a primitive type must but cannot be automatically promoted to a
  supertype.

  @since 1.0
*)

exception InvalidCodepoint of {
  loc:       Loc.t;  (** Location of the codepoint *)
  lexeme:    string; (** The lexeme representing the codepoint *)
  codepoint: int;    (** The invalid codepoint *)
}
(**
  Raised when a Unicode code point is not in the valid rage.

  @since 1.0
*)

(** {2 Desugaring} *)

val desug_ty : Annot.ty Env.t -> Syntax.ty -> (Annot.ty -> 'a) -> 'a
(**
  Desugar a type.

  @param env The type environment
  @param ty The type to desugar
  @param kontinue The continuation the desugared type is passed to
  @return The result of the continuation
  @since 1.0
*)

val desug_un : Annot.ty Env.t -> Syntax.un -> Annot.ty -> (Annot.ty -> Annot.builtin -> 'a) -> 'a
(**
  Desugar a unary operator into a built-in function.

  @param env The type environment
  @param op The operator to desugar
  @param operand The type of the operand
  @param kontinue The continuation the result type and the built-in function are
    passed to
  @return The result of the continuation
  @since 1.0
*)

val desug_bin : Annot.ty Env.t -> Syntax.bin -> Annot.ty -> Annot.ty -> (Annot.ty -> Annot.builtin -> 'a) -> 'a
(**
  Desugar a binary operator into a built-in function.

  @param env The type environment
  @param op The operator to desugar
  @param lhs The type of the left operand
  @param rhs The type of the second operand
  @param kontinue The continuation the result type and the desugared built-in
    function are passed to
  @return The result of the continuation
  @since 1.0
*)

val desug_rune : Annot.ty Env.t -> Syntax.rune -> (Uchar.t -> 'a) -> 'a
(**
  Desugar a rune literal.

  @param env The type environment
  @param r The rune literal
  @param kontinue The continuation the desugared rune is passed to
  @return The result of the continuation
  @since 1.0
*)

val desug_str : Annot.ty Env.t -> Syntax.str list list -> (string -> 'a) -> 'a
(**
  Desugar a string.  A string is given as a list of lines, each of which is
  given as a list of segments.

  @param env The type environment
  @param str The string segments
  @param kontinue The continuation the desugared string is passed to
  @return The result of the continuation
  @since 1.0
*)

val desug_expr : Annot.ty Env.t -> Syntax.expr -> (Annot.ty -> Annot.expr -> 'a) -> 'a
(**
  Desugar an expression.

  @param env The type environment
  @param expr The expression to desugar
  @param kontinue The continuation the desugared expression and its type are
    passed to
  @return The result of the continuation
  @since 1.0
*)

val desug_patt : Annot.ty Env.t -> Syntax.patt -> Annot.ty -> (Annot.ty Env.t -> Annot.patt -> 'a) -> 'a
(**
  Desugar a pattern.

  @param env The type environment
  @param patt The pattern to desugar
  @param ty The type of values the pattern must match
  @param kontinue The continuation the desugared pattern and the (possibly
    updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)

val desug_binding : Annot.ty Env.t -> Syntax.binding -> (Annot.ty Env.t -> Annot.binding -> 'a) -> 'a
(**
  Desugar a value binding.

  @param env The type environment
  @param binding The binding to desugar
  @param kontinue The continuation the desugared binding and a (possibly
    updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)

val desug_top : Annot.ty Env.t -> Syntax.top -> (Annot.ty Env.t -> Annot.top -> 'a) -> 'a
(**
  Desugar a top-level binding.

  @param env The type environment
  @param top The top-level binding to desugar
  @param kontinue The continuation the desugared top-level binding and a
    (possibly updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)

val desug_file : Annot.ty Env.t -> Syntax.file -> (Annot.ty Env.t -> Annot.top list -> 'a) -> 'a
(**
  Desugar a file.

  @param env The type environment
  @param file The file to desugar
  @param kontinue The continuation the desugred top-level bindings and a
    (possibly updated) type environment are passed to
  @return The result of the continuation
  @since 1.0
*)
