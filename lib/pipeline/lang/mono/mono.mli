(** {1 Monomorphic} *)

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
(**
  Types

  @since 1.0
*)

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
(**
  Built-in function arity

  @since 1.0
*)

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
  | BuiltinLsl of {
      ty: ty (** Operand type *)
    } (** Logical Shift Left *)
  | BuiltinLsr of {
      ty: ty (** Operand type *)
    } (** Logical Shift Right *)
  | BuiltinAsl of {
      ty: ty (** Operand type *)
    } (** Arithmetic Shift Left *)
  | BuiltinAsr of {
      ty: ty (** Operand type *)
    } (** Arithmetic Shift Right *)
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
(**
  Built-in functions

  @since 1.0
*)

type patt = private
  | PattGround (** Ground *)
  | PattVar of {
      id: Sym.t (** Identifier *)
    } (** Variable *)
(**
  Patterns

  @since 1.0
*)

type atom = private
  | AtomBool of {
      value: bool (** Value *)
    } (** Booleans *)
  | AtomInt of {
      value: int32 (** Value *)
    } (** Integers *)
  | AtomLong of {
      value: int64 (** Value *)
    } (** Longs *)
  | AtomFloat of {
      value: float (** Value *)
    } (** Floats *)
  | AtomDouble of {
      value: float (** Value *)
    } (** Doubles *)
  | AtomRune of {
      value: Uchar.t (** Value *)
    } (** Runes *)
  | AtomString of {
      value: string; (** UTF-8 encoded value *)
      len:   int     (** Length in runes *)
    } (** Strings *)
  | AtomIdent of {
      id: Sym.t (** Identifier *)
    } (** Identifier *)
(**
  Atomic values

  @since 1.0
*)

type expr = private
  | ExprBuiltin of {
      fn:   builtin;  (** Built-in function *)
      args: atom list (** Arguments *)
    } (** Built-in Function Application *)
  | ExprAtom of {
      atom: atom (** Atomic Value *)
    } (** Atomic Expression *)
(**
  Expressions

  @since 1.0
*)

type binding = private
  | Binding of {
      patt:  patt; (** Pattern *)
      ty:    ty;   (** Type annotation *)
      value: expr  (** Value expression *)
    } (** Binding *)
(**
  Bindings

  @since 1.0
*)

type term = private
  | TermLet of {
      binding: binding; (** Binding *)
      scope:   term    (** Scope *)
    } (** Let Binding *)
  | TermExpr of {
      expr: expr (** Expression *)
    } (** Expression Term *)
(**
  Term values

  @since 1.0
*)

type top = private
  | TopLet of {
      binding: binding (** Binding *)
    } (** Let Binding *)
(**
  Top-level bindings

  @since 1.0
*)

(** {2 Constructors} *)

(** {3 Types} *)

val ty_bool : ty
(**
  The [Bool] type.

  @since 1.0
*)

val ty_int : ty
(**
  The [Int] type.

  @since 1.0
*)

val ty_long : ty
(**
  The [Long] type.

  @since 1.0
*)

val ty_float : ty
(**
  The [Float] type.

  @since 1.0
*)

val ty_double : ty
(**
  The [Double] type.

  @since 1.0
*)

val ty_rune : ty
(**
  The [Rune] type.

  @since 1.0
*)

val ty_string : ty
(**
  The [String] type.

  @since 1.0
*)

(** {3 Built-In Functions} *)

(** {4 Arities} *)

val arity_fixed : ty list -> ty -> arity
(**
  Construct a fixed arity for a built-in function.

  @param args The types of the arguments, in order
  @param res The type of the result
  @return A fixed arity
  @since 1.0
*)

val arity_var : ty -> ty -> arity
(**
  Construct a variable arity for a built-in function.

  @param args The type of all arguments
  @param res The type of the result
  @return A variable arity
  @since 1.0
*)

(** {4 Exceptions} *)

exception NotIntegral of {
  ty: ty; (** The non-integral type *)
}
(**
  Raised when a type is not an integral type.

  @since 1.0
*)

exception NotFloatingPoint of {
  ty: ty; (** The non-floating-point type *)
}
(**
  Raised when a type is not a floating-point type.

  @since 1.0
*)

exception NotNumeric of {
  ty: ty; (** The non-numeric type *)
}
(**
  Raised when a type is not a numeric type.

  @since 1.0
*)

exception UnsupportedPromotion of {
  sub: ty; (** The unpromotable subtype *)
  sup: ty; (** The target supertype *)
}
(**
  Raised when the promotion from a subtype to a supertype cannot be performed.

  @since 1.0
*)

exception UnsupportedConcatType of {
  ty: ty; (** The type of values that cannot be concatenated. *)
}
(**
  Raised when the arguments to a {!Concat} built-in function are not of one of
  the supported types.

  @since 1.0
*)

(** {4 Constructors} *)

val builtin_struct_eq : ty -> builtin
(**
  Construct a structural equality built-in function.

  @param ty The type of the operands
  @return A structural equality built-in function
  @since 1.0
*)

val builtin_struct_neq : ty -> builtin
(**
  Construct a structural inequality built-in function.

  @param ty The type of the operands
  @return A structural inequality built-in function
  @since 1.0
*)

val builtin_phys_eq : ty -> builtin
(**
  Construct a physical equality built-in function.

  @param ty The type of the operands
  @return A physical equality built-in function
  @since 1.0
*)

val builtin_phys_neq : ty -> builtin
(**
  Construct a physical inequality built-in function.

  @param ty The type of the operands
  @return A physical inequality built-in function
  @since 1.0
*)

val builtin_lt : ty -> builtin
(**
  Construct a less than built-in function.

  @param ty The type of the operands
  @return A less than built-in function
  @raise NotNumeric Raised if the type is not a numeric type as determined by
    {!Type.ty_is_numeric}
  @since 1.0
*)

val builtin_lte : ty -> builtin
(**
  Construct a less than or equal built-in function.

  @param ty The type of the operands
  @return A less than or equal built-in function
  @raise NotNumeric Raised if the type is not a numeric type as determined by
    {!Type.ty_is_numeric}
  @since 1.0
*)

val builtin_gt : ty -> builtin
(**
  Construct a greater than built-in function.

  @param ty The type of the operands
  @return A greater than built-in function
  @raise NotNumeric Raised if the type is not a numeric type as determined by
    {!Type.ty_is_numeric}
  @since 1.0
*)

val builtin_gte : ty -> builtin
(**
  Construct a greater than or equal built-in function.

  @param ty The type of the operands
  @return A greater than or equal built-in function
  @raise NotNumeric Raised if the type is not a numeric type as determined by
    {!Type.ty_is_numeric}
  @since 1.0
*)

val builtin_lsl : ty -> builtin
(**
  Construct a logical shift left built-in function.

  @param ty The type of the operands
  @return A logical shift left built-in function
  @raise NotIntegral Raised if the type is not an integral type as determined by
    {!Type.ty_is_integral}
  @since 1.0
*)

val builtin_lsr : ty -> builtin
(**
  Construct a logical shift right built-in function.

  @param ty The type of the operands
  @return A logical shift right built-in function
  @raise NotIntegral Raised if the type is not an integral type as determined by
    {!Type.ty_is_integral}
  @since 1.0
*)

val builtin_asl : ty -> builtin
(**
  Construct an arithmetic shift left built-in function.

  @param ty The type of the operands
  @return A arithmetic shift left built-in function
  @raise NotIntegral Raised if the type is not an integral type as determined by
    {!Type.ty_is_integral}
  @since 1.0
*)

val builtin_asr : ty -> builtin
(**
  Construct an arithmetic shift right built-in function.

  @param ty The type of the operands
  @return A arithmetic shift right built-in function
  @raise NotIntegral Raised if the type is not an integral type as determined by
    {!Type.ty_is_integral}
  @since 1.0
*)

val builtin_add : ty -> builtin
(**
  Construct an addition built-in function.

  @param ty The type of the operands
  @return An addition built-in function
  @raise NotNumeric Raised if the type is not a numeric type as determined by
    {!Type.ty_is_numeric}
  @since 1.0
*)

val builtin_sub : ty -> builtin
(**
  Construct a subtraction built-in function.

  @param ty The type of the operands
  @return A subtraction built-in function
  @raise NotNumeric Raised if the type is not a numeric type as determined by
    {!Type.ty_is_numeric}
  @since 1.0
*)

val builtin_mul : ty -> builtin
(**
  Construct a multiplication built-in function.

  @param ty The type of the operands
  @return A multiplication built-in function
  @raise NotNumeric Raised if the type is not a numeric type as determined by
    {!Type.ty_is_numeric}
  @since 1.0
*)

val builtin_div : ty -> builtin
(**
  Construct a division built-in function.

  @param ty The type of the operands
  @return A division built-in function
  @raise NotNumeric Raised if the type is not a numeric type as determined by
    {!Type.ty_is_numeric}
  @since 1.0
*)

val builtin_mod : ty -> builtin
(**
  Construct a modulus built-in function.

  @param ty The type of the operands
  @return A modulus built-in function
  @raise NotIntegral Raised if the type is not an integral type as determined by
    {!Type.ty_is_integral}
  @since 1.0
*)

val builtin_exp : ty -> builtin
(**
  Construct an exponentiation built-in function.

  @param ty The type of the operands
  @return An exponentiation built-in function
  @raise NotFloatingPoint Raised if the type is not a floating-point type as
    determined by {!Type.ty_is_floating_point}
  @since 1.0
*)

val builtin_neg : ty -> builtin
(**
  Construct a negation built-in function.

  @param ty The type of the operands
  @return A negation built-in function
  @raise NotNumeric Raised if the type is not a numeric type as determined by
    {!Type.ty_is_numeric}
  @since 1.0
*)

val builtin_bit_and : ty -> builtin
(**
  Construct a bitwise AND built-in function.

  @param ty The type of the operands
  @return A bitwise AND built-in function
  @raise NotIntegral Raised if the type is not an integral type as determined by
    {!Type.ty_is_integral}
  @since 1.0
*)

val builtin_bit_or : ty -> builtin
(**
  Construct a bitwise OR built-in function.

  @param ty The type of the operands
  @return A bitwise OR built-in function
  @raise NotIntegral Raised if the type is not an integral type as determined by
    {!Type.ty_is_integral}
  @since 1.0
*)

val builtin_bit_not : ty -> builtin
(**
  Construct a bitwise NOT built-in function.

  @param ty The type of the operands
  @return A bitwise NOT built-in function
  @raise NotIntegral Raised if the type is not an integral type as determined by
    {!Type.ty_is_integral}
  @since 1.0
*)

val builtin_bit_xor : ty -> builtin
(**
  Construct a bitwise XOR built-in function.

  @param ty The type of the operands
  @return A bitwise XOR built-in function
  @raise NotIntegral Raised if the type is not an integral type as determined by
    {!Type.ty_is_integral}
  @since 1.0
*)

val builtin_log_not : builtin
(**
  The logical NOT built-in function.

  @since 1.0
*)

val builtin_promote : ty -> ty -> builtin
(**
  Construct a type promotion built-in function.

  @param sub The promotable subtype
  @param sup The target supertype
  @return A type promotion built-in function
  @raise UnsupportedPromotion Raised if the subtype cannot be promoted to the
    supertype
  @since 1.0
*)

val builtin_concat : ty -> builtin
(**
  Construct a concatenation built-in function.

  @param ty The type of values to concatenate
  @return A concatenation built-in function
  @raise UnsupportedConcatType Raised if the type is not an allowed type for
    concatenation.
  @since 1.0
*)

(** {3 Patterns} *)

val patt_ground : patt
(**
  The ground ([_]) pattern.

  @since 1.0
*)

val patt_var : Sym.t -> patt
(**
  Construct a variable pattern binding an identifier.

  @param id The identifier to bind
  @return A variable pattern binding the identifier
  @since 1.0
*)

(** {3 Atoms} *)

val atom_bool : bool -> atom
(**
  Construct a boolean literal atom from a value.

  @param value The boolean value
  @return A boolean literal atom
  @since 1.0
*)

val atom_int : int32 -> atom
(**
  Construct an integer literal atom from a value.

  @param value The 32-bit integer value
  @return An integer literal atom
  @since 1.0
*)

val atom_long : int64 -> atom
(**
  Construct a long literal atom from a value.

  @param value The 64-integer value
  @return An long literal atom
  @since 1.0
*)

val atom_float : float -> atom
(**
  Construct a float literal atom from a value.

  @param value The floating-point value
  @return A float literal atom
  @since 1.0
*)

val atom_double : float -> atom
(**
  Construct a double literal atom from a value.

  @param value The floating-point value
  @return A double literal
  @since 1.0
*)

val atom_rune : Uchar.t -> atom
(**
  Construct a rune literal atom from a value.

  @param value The UTF-8 character value
  @return A rune literal atom
  @since 1.0
*)

val atom_string : string -> atom
(**
  Construct a string literal atom from a value.  The value is normalized and the
  length is computed.

  @param value The UTF-8 string value
  @return A string literal atom
  @since 1.0
*)

val atom_ident : Sym.t -> atom
(**
  Construct an identifier atom from an identifier.

  @param id The identifier's symbol
  @return An identifier atom
  @since 1.0
*)

(** {3 Expressions} *)

val expr_builtin : builtin -> atom list -> expr
(**
  Construct an application of a built-in function to atomic arguments.

  @param fn The built-in function to apply
  @param args The atomic arguments to apply the function to
  @return A built-in function application
  @since 1.0
*)

val expr_atom : atom -> expr
(**
  Construct an atomic value expression.

  @param atom The atomic value
  @return An atomic value expression
  @since 1.0
*)

(** {3 Bindings} *)

val binding : patt -> ty -> expr -> binding
(**
  Construct a binding that binds an expression of a type to a pattern.

  @param patt The pattern to bind to
  @param ty The type of the binding
  @param expr The value to bind
  @return A value binding
  @since 1.0
*)

(** {3 Terms} *)

val term_let : binding -> term -> term
(**
  Construct a let binding a term in a scope.

  @param binding The let binding
  @param scope The scope of the binding
  @return A let binding
  @since 1.0
*)

val term_expr : expr -> term
(**
  Construct an expression value term.

  @param expr The expression value
  @return An expression value term
  @since 1.0
*)

(** {3 Top-Level Bindings} *)

val top_let : binding -> top
(**
  Construct a top-level value binding.

  @param binding The binding
  @return A top-level value binding
  @since 1.0
*)

(** {2 Operations} *)

(** {3 Types} *)

val ty_equal : ty -> ty -> bool
(**
  Test if two types are equal.

  @param ty The first type
  @param ty' The second type
  @return [true] if the types are equal, [false] otherwise
  @since 1.0
*)

val ty_is_integral : ty -> bool
(**
  Test if a type is integral (either [Int] or [Long]).

  @param ty The type
  @return [true] if the type is integral, [false] otherwise
  @since 1.0
*)

val ty_is_floating_point : ty -> bool
(**
  Test if a type is floating-point (either [Float] or [Double]).

  @param ty The type
  @return [true] if the type is floating-point, [false] otherwise
  @since 1.0
*)

val ty_is_numeric : ty -> bool
(**
  Test if a type is a numeric (either integral or floating-point).

  @param ty The type
  @param [true] if the type is numeric, [false] otherwise
  @since 1.0
*)

val ty_is_logical : ty -> bool
(**
  Test if a type is logical (a [Bool]).

  @param ty The type
  @return [true] if the type is logical, [false] otherwise
  @since 1.0
*)

(** {3 Pretty Printing} *)

val pp_ty : formatter -> ty -> unit
(**
  Pretty-print a type to a formatter.

  @param fmt The formatter to print to
  @param ty The type to print
  @since 1.0
*)

val pp_arity : formatter -> arity -> unit
(**
  Pretty-print an arity to a formatter.

  @param fmt The formatter to print to
  @param arity The arity to print
  @since 1.0
*)

val pp_builtin : formatter -> builtin -> unit
(**
  Pretty-print a built-in function to a formatter.

  @param fmt The formatter to print to
  @param builtin The built-in function to print
  @since 1.0
*)

val pp_patt : formatter -> patt -> unit
(**
  Pretty-print a pattern to a formatter.

  @param fmt The formatter to print to
  @param patt The pattern to print
  @since 1.0
*)

val pp_atom : formatter -> atom -> unit
(**
  Pretty-print an atom to a formatter.

  @param fmt The formatter to print to
  @param atom The atom to print
  @since 1.0
*)

val pp_expr : formatter -> expr -> unit
(**
  Pretty-print an expression to a formatter.

  @param fmt The formatter to print to
  @param expr The expression to print
  @since 1.0
*)

val pp_binding : formatter -> binding -> unit
(**
  Pretty-print a value binding to a formatter.

  @param fmt The formatter to print to
  @param binding The binding to print
  @since 1.0
*)

val pp_term : formatter -> term -> unit
(**
  Pretty-print a term to a formatter.

  @param fmt The formatter to print to
  @param term The term to print
  @since 1.0
*)

val pp_top : formatter -> top -> unit
(**
  Pretty-print a top-level let binding to a formatter.

  @param fmt The formatter to print to
  @param top The top-level let binding to print
  @since 1.0
*)

(** {3 Type Checking} *)

exception UnboundIdentifier of {
  id: Sym.t; (** The unbound identifier *)
}
(**
  Raised when an identifier is unbound.

  @since 1.0
*)

exception MismatchedTypes of {
  inferred:  ty; (** The inferred type *)
  annotated: ty; (** The annotated type *)
}
(**
  Raised when the inferred type of an expression disagrees with the type
  annotation.

  @since 1.0
*)

exception InvalidArity of {
  expected: int; (** The expected arity *)
  actual:   int; (** The actual arity *)
}
(**
  Raised when a built-in function is applied to the incorrect number of
  arguments.

  @since 1.0
*)

val check_builtin : ty Env.t -> builtin -> (arity -> 'a) -> 'a
(**
  Determine the arity of a built-in function.

  @param env The type checking environment
  @param builtin The builtin
  @param kontinue The continuation the arity is passed to
  @return The result of the continuation
  @since 1.0
*)

val check_atom : ty Env.t -> atom -> (ty -> 'a) -> 'a
(**
  Infer the type of an atomic value.

  @param env The type checking environment
  @param atom The atomic value
  @param kontinue The continuation the type is passed to
  @return The result of the continuation
  @since 1.0
*)

val check_expr : ty Env.t -> expr -> (ty -> 'a) -> 'a
(**
  Check that an expression is well-typed.

  @param env The type checking environment
  @param expr The expression
  @param kontinue The continuation the type of the expression is passed to
  @return The result of the continuation
  @since 1.0
*)

val check_patt : ty Env.t -> patt -> ty -> (ty Env.t -> 'a) -> 'a
(**
  Check that a pattern matches against values of a particular type.

  @param env The type checking environment
  @param patt The pattern
  @param ty The type to match against
  @param kontinue The continuation the (possibly updated) environment is passed
    to
  @return The result of the continuation
  @since 1.0
*)

val check_binding : ty Env.t -> binding -> (ty Env.t -> 'a) -> 'a
(**
  Check that a value binding is well typed and bind the value in the
  environment.

  @param env The type checking environment
  @param binding The binding
  @param kontinue The continuation to pass the (possibly updated) environment to
  @return The result of the continuation
  @since 1.0
*)

val check_term : ty Env.t -> term -> (ty Env.t -> ty -> 'a) -> 'a
(**
  Check that a term is well-typed and bind any values.

  @param env The type checking environment
  @param expr The term
  @param kontinue The continuation the type of the term and a (possibly updated)
    environment is passed to
  @return The result of the continuation
  @since 1.0
*)

val check_top : ty Env.t -> top -> (ty Env.t -> 'a) -> 'a
(**
  Check that a top-level binding is well typed and bind the value in the
  environment.

  @param env The type checking environment
  @param top The top-level binding
  @param kontinue The continuation to pass the (possibly updated) environment to
  @return The result of the continuation
  @since 1.0
*)
