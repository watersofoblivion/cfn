(** {1 Annotated Syntax} *)

open Format

open Common

(** {2 Syntax} *)

type ty = private
  | TyBool   (** Boolean *)
  | TyInt    (** Integer *)
  | TyLong   (** Long *)
  | TyFloat  (** Float *)
  | TyDouble (** Double *)
  | TyRune   (** Rune *)
  | TyString (** String *)
(** Types *)

type builtin = private
  | Add of {
      ty: ty (** Operand type *)
    } (** Addition *)
  | Sub of {
      ty: ty (** Operand type *)
    } (** Subtraction *)
  | Mul of {
      ty: ty (** Operand type *)
    } (** Multiplication *)
  | Div of {
      ty: ty (** Operand type *)
    } (** Division *)
  | Mod of {
      ty: ty (** Operand type *)
    } (** Modulus *)
  | Exp of {
      ty: ty (** Operand type *)
    } (** Exponentiation *)
  | Promote of {
      sub: ty; (** Subtype *)
      sup: ty  (** Supertype *)
    } (** Type Promotion *)
  | Concat of {
      ty: ty (** Operand type *)
    } (** Concatenation *)
(** Builtin Functions *)

type patt = private
  | PattGround (** Ground *)
  | PattVar of {
      id: Sym.t  (** Identifier *)
    } (** Variable *)
(** Patterns *)

and expr = private
  | Bool of {
      value: bool (** Value *)
    } (** Booleans *)
  | Int of {
      value: int32 (** Value *)
    } (** Integers *)
  | Long of {
      value: int64 (** Value *)
    } (** Longs *)
  | Float of {
      value: float (** Value *)
    } (** Floats *)
  | Double of {
      value: float (** Value *)
    } (** Doubles *)
  | Rune of {
      value: Uchar.t (** Value *)
    } (** Runes *)
  | String of {
      value: string; (** UTF-8 encoded value *)
      len:   int     (** Length in runes *)
    } (** Strings *)
  | Ident of {
      id: Sym.t (** Identifier *)
    } (** Identifiers *)
  | Builtin of {
      fn:   builtin; (** Built-in function *)
      args: expr list (** Arguments *)
    } (** Function Application *)
(** Expressions *)

and binding = private
  | Binding of {
      patt:  patt; (** Pattern *)
      ty:    ty;   (** Type annotation *)
      value: expr; (** Value expression *)
    }
(** Bindings *)

type top = private
  | Let of {
      binding: binding (** Binding *)
    } (** Let Binding *)
(** Top-Level Bindings *)

(** {2 Constructors} *)

(** {3 Types} *)

val ty_bool : ty
(** [ty_bool] constructs a boolean type. *)

val ty_int : ty
(** [ty_int] constructs an integer type. *)

val ty_long : ty
(** [ty_long] constructs a long type. *)

val ty_float : ty
(** [ty_float] constructs a float type. *)

val ty_double : ty
(** [ty_double] constructs a double type. *)

val ty_rune : ty
(** [ty_rune] constructs a rune type. *)

val ty_string : ty
(** [ty_string] constructs a string type. *)

(** {3 Built-In Functions} *)

exception NotIntegral of ty
(** [NotIntegral ty] is raised when the type [ty] is not an integral type as
    determined by {!Type.ty_is_numeric}. *)

exception NotFloatingPoint of ty
(** [NotFloatingPoint ty] is raised when the type [ty] is not a floating-point
    type as determined by {!Type.ty_is_floating_point}. *)

exception NotNumeric of ty
(** [NotNumeric ty] is raised when the type [ty] is not a numeric type as
    determined by {!Type.ty_is_numeric}. *)

exception UnsupportedPromotion of ty * ty
(** [UnsupportedPromotion (sub, sup)] is raised when the promotion from the
    subtype [sub] to the supertype [sup] cannot be performed. *)

exception UnsupportedConcatType of ty
(** [UnsupportedConcatType ty] is raised when the type of a {!Concat} call is
    not one of the supported types. *)

val builtin_add : ty -> builtin
(** [builtin_add ty] constructs an addition builtin operating on values of type
    [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined by
    {!Type.ty_is_numeric}. *)

val builtin_sub : ty -> builtin
(** [builtin_sub ty] constructs a subtration builtin operating on values of type
    [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined by
    {!Type.ty_is_numeric}. *)

val builtin_mul : ty -> builtin
(** [builtin_mul ty] constructs a multiplication builtin operating on values of
    type [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined
    by {!Type.ty_is_numeric}. *)

val builtin_div : ty -> builtin
(** [builtin_div ty] constructs a division builtin operating on values of type
    [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined by
    {!Type.ty_is_numeric}. *)

val builtin_mod : ty -> builtin
(** [builtin_mod ty] constructs a modulus builtin operating on values of type
    [ty].  Raises {!NotIntegral} if [ty] is not an integral type as determined
    by {!Type.ty_is_integral}. *)

val builtin_exp : ty -> builtin
(** [builtin_exp ty] constructs an exponentiation builtin operating on values of
    type [ty].  Raises {!NotFloatingPoint} if [ty] is not a floating-point type
    as determined by {!Type.ty_is_floating_point}. *)

val builtin_promote : ty -> ty -> builtin
(** [builtin_promote sub sup] constructs a promotion builtin promoting values of
    the subtype [sub] to values of the supertype [sup].  Raises
    {!UnsupportedPromotion} if promoting from [sub] to [sup] is not supported. *)

val builtin_concat : ty -> builtin
(** [builtin_concat ty] constructs a concatenation builtin operating on values
    of type [ty].  Raises {!UnsupportedConcatType} if [ty] is not one of the
    allowed types for concatenation. *)

(** {3 Patterns} *)

val patt_ground : patt
(** [patt_ground] constructs a ground pattern. *)

val patt_var : Sym.t -> patt
(** [patt_var id] constructs a variable pattern binding the identifier [id]. *)

(** {3 Expressions} *)

val expr_bool : bool -> expr
(** [expr_bool value] constructs a boolean literal with value [value]. *)

val expr_int : int32 -> expr
(** [expr_int value] constructs an integer literal with value [value]. *)

val expr_long : int64 -> expr
(** [expr_long value] constructs a long literal with value [value]. *)

val expr_float : float -> expr
(** [expr_float value] constructs a float literal with value [value]. *)

val expr_double : float -> expr
(** [expr_double value] constructs a double literal with value [value]. *)

val expr_rune : Uchar.t -> expr
(** [expr_rune value] constructs a rune literal with value [value]. *)

val expr_string : string -> expr
(** [expr_string value] constructs a string literal with value [value].  The
    value is normalized and the length is computed. *)

val expr_ident : Sym.t -> expr
(** [expr_ident id] constructs an identifier literal with the identifier [id]. *)

val expr_builtin : builtin -> expr list -> expr
(** [expr_builtin fn args] constructs an application of the built-in function
    [fn] to the arguments [args]. *)

(** {3 Bindings} *)

val binding : patt -> ty -> expr -> binding
(** [binding patt ty expr] constructs a binding that binds the value [expr] of
    type [ty] to the pattern [patt]. *)

(** {3 Top-Level Bindings} *)

val top_let : binding -> top
(** [top_let patt ty value] constructs a top-level let binding of the binding
    [binding]. *)

(** {2 Operations} *)

(** {3 Types} *)

val ty_equal : ty -> ty -> bool
(** [ty_equal ty ty'] tests if type [ty] is equal to type [ty']. *)

val ty_is_integral : ty -> bool
(** [ty_is_integral ty] tests if type [ty] is an integral ([Int] or [Long])
    type. *)

val ty_is_floating_point : ty -> bool
(** [ty_is_floating_point ty] tests if type [ty] is a floating-point ([Float] or
    [Double]) type. *)

val ty_is_numeric : ty -> bool
(** [ty_is_numeric ty] tests if type [ty] is a numeric (integral or
    floating-point) type. *)

(** {3 Pretty Printing} *)

val pp_ty : formatter -> ty -> unit
(** [pp_ty fmt ty] pretty-prints the type [ty] to the formatter [fmt]. *)

val pp_builtin : formatter -> builtin -> unit
(** [pp_builtin fmt builtin] pretty-prints the application of the built-in
    function [builtin] to the formatter [fmt]. *)

val pp_patt : formatter -> patt -> unit
(** [pp_patt fmt patt] pretty-prints the pattern [patt] to the formatter [fmt]. *)

val pp_expr : formatter -> expr -> unit
(** [pp_expr fmt expr] pretty-prints the expression [expr] to the formatter
    [fmt]. *)

val pp_binding : formatter -> binding -> unit
(** [pp_binding fmt binding] pretty-prints the binding [binding] to the
    formatter [fmt]. *)

val pp_top : formatter -> top -> unit
(** [pp_top fmt top] pretty-prints the top-level expression [top] to the
    formatter [fmt]. *)

(** {3 Type Checking} *)

exception UnboundIdentifier of Sym.t
(** [UnboundIdentifier id] is raised when the identifier [id] is unbound. *)

exception MismatchedTypes of expr * ty * ty
(** [MismatchedTypes (expr, inferred, annotated)] is raised when the [inferred]
    type of the expression [expr] and the [annotated] type disagree. *)

exception InvalidArity of int * int
(** [InvalidArity (expected, actual)] is raised when a built-in function of
    arity [expected] is applied to [actual] arguments. *)

exception UnsupportedConcatArg of expr * ty * ty
(** [UnsupportedConcatArg (expr, inferred, expected)] is raised when the
    argument [expr] to a {!Concat} call is of type [inferred] instead of the
    type [expected]. *)

val check_builtin : ty Env.t -> builtin -> expr list -> (ty -> 'a) -> 'a
(** [check_builtin env builtin args kontinue] type-checks the application of the
    built-in function [builtin] to the arguments [args] in the environment
    [env].  The result type of the application is passed to the continuation
    [kontinue]. *)

val check_patt : ty Env.t -> patt -> ty -> (ty Env.t -> 'a) -> 'a
(** [check_patt env patt ty kontinue] type-checks the pattern [patt] against the
    type [ty] in the environment [env].  A (possibly updated) environment is
    passed to the continuation [kontinue]. *)

val check_expr : ty Env.t -> expr -> (ty -> 'a) -> 'a
(** [check_expr env expr kontinue] type-checks the expression [expr] in the
    environment [env].  The type of the expression is passed to the continuation
    [kontinue]. *)

val check_binding : ty Env.t -> binding -> (ty Env.t -> 'a) -> 'a
(** [check_binding env binding kontinue] type-checks the binding [binding] in
    the environment [env].  A (possibly updated) environment is passed to the
    continuation [kontinue]. *)

val check_top : ty Env.t -> top -> (ty Env.t -> 'a) -> 'a
(** [check_top env top kontinue] type-checks the top-level expression [top] in
    the environment [env].  A (possibly updated) environment is passed to the
    continuation [kontinue]. *)
