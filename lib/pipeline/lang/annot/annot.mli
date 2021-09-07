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

type arity =
  | ArityFixed of {
      arity: int;     (** Arity *)
      args:  ty list; (** Argument types *)
      res:   ty       (** Result type *)
    } (** Fixed Arity *)
  | ArityVar of {
      args: ty; (** Argument type *)
      res:  ty  (** Result type *)
    } (** Variable Arity *)
(* Builtin Arity *)

type builtin = private
  | BuiltinStructEq of {
      ty: ty (** Operand type *)
    } (** Structural Equality *)
  | BuiltinStructNeq of {
      ty: ty (** Operand type *)
    } (** Structural Inequality *)
  | BuiltinPhysEq of {
      ty: ty (** Operand type *)
    } (** Physical Equality *)
  | BuiltinPhysNeq of {
      ty: ty (** Operand type *)
    } (** Physical Inequality *)
  | BuiltinLt of {
      ty: ty (** Operand type *)
    } (** Less Than *)
  | BuiltinLte of {
      ty: ty (** Operand type *)
    } (** Less Than or Equal *)
  | BuiltinGt of {
      ty: ty (** Operand type *)
    } (** Greater Than *)
  | BuiltinGte of {
      ty: ty (** Operand type *)
    } (** Greater Than or Equal *)
  | BuiltinAdd of {
      ty: ty (** Operand type *)
    } (** Addition *)
  | BuiltinSub of {
      ty: ty (** Operand type *)
    } (** Subtraction *)
  | BuiltinMul of {
      ty: ty (** Operand type *)
    } (** Multiplication *)
  | BuiltinDiv of {
      ty: ty (** Operand type *)
    } (** Division *)
  | BuiltinMod of {
      ty: ty (** Operand type *)
    } (** Modulus *)
  | BuiltinExp of {
      ty: ty (** Operand type *)
    } (** Exponentiation *)
  | BuiltinNeg of {
      ty: ty (** Operand type *)
    } (** Negation *)
  | BuiltinBitAnd of {
      ty: ty (** Operand type *)
    } (** Bitwise AND *)
  | BuiltinBitOr of {
      ty: ty (** Operand type *)
    } (** Bitwise OR *)
  | BuiltinBitNot of {
      ty: ty (** Operand type *)
    } (** Bitwise NOT *)
  | BuiltinBitXor of {
      ty: ty (** Operand type *)
    } (** Bitwise XOR *)
  | BuiltinLogNot (** Logical NOT *)
  | BuiltinPromote of {
      sub: ty; (** Subtype *)
      sup: ty  (** Supertype *)
    } (** Type Promotion *)
  | BuiltinConcat of {
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
  | ExprBool of {
      value: bool (** Value *)
    } (** Booleans *)
  | ExprInt of {
      value: int32 (** Value *)
    } (** Integers *)
  | ExprLong of {
      value: int64 (** Value *)
    } (** Longs *)
  | ExprFloat of {
      value: float (** Value *)
    } (** Floats *)
  | ExprDouble of {
      value: float (** Value *)
    } (** Doubles *)
  | ExprRune of {
      value: Uchar.t (** Value *)
    } (** Runes *)
  | ExprString of {
      value: string; (** UTF-8 encoded value *)
      len:   int     (** Length in runes *)
    } (** Strings *)
  | ExprIdent of {
      id: Sym.t (** Identifier *)
    } (** Identifiers *)
  | ExprBuiltin of {
      fn:   builtin;  (** Built-in function *)
      args: expr list (** Arguments *)
    } (** Function Application *)
  | ExprLet of {
      binding: binding; (** Binding *)
      scope:   expr     (** Scope *)
    } (** Let Binding *)
(** Expressions *)

and binding = private
  | Binding of {
      patt:  patt; (** Pattern *)
      ty:    ty;   (** Type annotation *)
      value: expr; (** Value expression *)
    }
(** Bindings *)

type top = private
  | TopLet of {
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

(** {4 Arities} *)

val arity_fixed : ty list -> ty -> arity
(** [arity_fixed args res] constructs a fixed arity with arguments of types
    [args] and result of type [res]. *)

val arity_var : ty -> ty -> arity
(** [arity_var args res] constructs a variable arity with arguments of type
    [args] and a result of type [res]. *)

(** {4 Exceptions} *)

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

(** {4 Constructors} *)

val builtin_struct_eq : ty -> builtin
(** [builtin_struct_eq ty] constructs a structural equality builtin operating on
    values of type [ty]. *)

val builtin_struct_neq : ty -> builtin
(** [builtin_struct_neq ty] constructs a structural inequality builtin operating
    on values of type [ty]. *)

val builtin_phys_eq : ty -> builtin
(** [builtin_phys_eq ty] constructs a physical equality builtin operating on
    values of type [ty]. *)

val builtin_phys_neq : ty -> builtin
(** [builtin_phys_neq ty] constructs a physical inequality builtin operating
    on values of type [ty]. *)

val builtin_lt : ty -> builtin
(** [builtin_lt ty] constructs a less than builtin operating on values of type
    [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined by
    {!Type.ty_is_numeric}. *)

val builtin_lte : ty -> builtin
(** [builtin_lte ty] constructs a less than or equal builtin operating on values
    of type [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as
    determined by {!Type.ty_is_numeric}. *)

val builtin_gt : ty -> builtin
(** [builtin_gt ty] constructs a greater than builtin operating on values of
    type [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined
    by {!Type.ty_is_numeric}. *)

val builtin_gte : ty -> builtin
(** [builtin_gte ty] constructs a greater than or equal builtin operating on
    values of type [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as
    determined by {!Type.ty_is_numeric}. *)

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

val builtin_neg : ty -> builtin
(** [builtin_neg ty] constructs a negation builtin operating on values of type
    [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined by
    {!Type.ty_is_numeric}. *)

val builtin_bit_and : ty -> builtin
(** [builtin_bit_and ty] constructs a bitwise AND builtin operating on values of
    type [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined
    by {!Type.ty_is_numeric}. *)

val builtin_bit_or : ty -> builtin
(** [builtin_bit_or ty] constructs a bitwise OR builtin operating on values of
    type [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined
    by {!Type.ty_is_numeric}. *)

val builtin_bit_not : ty -> builtin
(** [builtin_bit_not ty] constructs a bitwise NOT builtin operating on values of
    type [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined
    by {!Type.ty_is_numeric}. *)

val builtin_bit_xor : ty -> builtin
(** [builtin_bit_xor ty] constructs a bitwise XOR builtin operating on values of
    type [ty].  Raises {!NotNumeric} if [ty] is not a numeric type as determined
    by {!Type.ty_is_numeric}. *)

val builtin_log_not : builtin
(** [builtin_log_not] constructs a logical NOT builtin. *)

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

val expr_let : binding -> expr -> expr
(** [expr_let binding scope] constructs a let binding of [binding] in the scope
    [scope]. *)

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

val ty_is_logical : ty -> bool
(** [ty_is_logical ty] tests if type [ty] is a logical (boolean) type. *)

(** {3 Pretty Printing} *)

val pp_ty : formatter -> ty -> unit
(** [pp_ty fmt ty] pretty-prints the type [ty] to the formatter [fmt]. *)

val pp_arity : formatter -> arity -> unit
(** [pp_arity fmt arity] pretty-prints the arity [arity] to the formatter [fmt]. *)

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

val check_builtin : ty Env.t -> builtin -> (arity -> 'a) -> 'a
(** [check_builtin env builtin kontinue] gets the type of the built-in function
    [builtin] in the environment [env].  The arity the function is passed to the
    continuation [kontinue]. *)

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
