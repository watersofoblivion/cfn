(** {1 Abstract Syntax} *)

open Format

open Common

(** {2 Syntax} *)

type ty = private
  | TyConstr of {
      loc: Loc.t; (** Location *)
      id:  Sym.t  (** Constructor *)
    } (** Type Constructor *)
(**
  Types

  @since 1.0
*)

type un =
  | UnNeg of {
      loc: Loc.t (** Location *)
    } (** Negation *)
  | UnLogNot of {
      loc: Loc.t (** Location *)
    } (** Logical NOT *)
  | UnBitNot of {
      loc: Loc.t (** Location *)
    } (** Bitwise NOT *)
(**
  Unary operators

  @since 1.0
*)

type bin =
  | BinStructEq of {
      loc: Loc.t (** Location *)
    } (** Structural Equality *)
  | BinStructNeq of {
      loc: Loc.t (** Location *)
    } (** Structural Inequality *)
  | BinPhysEq of {
      loc: Loc.t (** Location *)
    } (** Physical Equality *)
  | BinPhysNeq of {
      loc: Loc.t (** Location *)
    } (** Physical Inequality *)
  | BinLt of {
      loc: Loc.t (** Location *)
    } (** Less Than *)
  | BinLte of {
      loc: Loc.t (** Location *)
    } (** Less Than or Equal *)
  | BinGt of {
      loc: Loc.t (** Location *)
    } (** Greater Than *)
  | BinGte of {
      loc: Loc.t (** Location *)
    } (** Greater Than or Equal *)
  | BinLsl of {
      loc: Loc.t (** Location *)
    } (** Logical Shift Left *)
  | BinLsr of {
      loc: Loc.t (** Location *)
    } (** Logical Shift Right *)
  | BinAsl of {
      loc: Loc.t (** Location *)
    } (** Arithmetic Shift Left *)
  | BinAsr of {
      loc: Loc.t (** Location *)
    } (** Arithmetic Shift Right *)
  | BinAdd of {
      loc: Loc.t (** Location *)
    } (** Addition *)
  | BinSub of {
      loc: Loc.t (** Location *)
    } (** Subtraction *)
  | BinMul of {
      loc: Loc.t (** Location *)
    } (** Multiplication *)
  | BinDiv of {
      loc: Loc.t (** Location *)
    } (** Division *)
  | BinMod of {
      loc: Loc.t (** Location *)
    } (** Modulus *)
  | BinExp of {
      loc: Loc.t (** Location *)
    } (** Exponentiation *)
  | BinLogAnd of {
      loc: Loc.t (** Location *)
    } (** Logical AND *)
  | BinLogOr of {
      loc: Loc.t (** Location *)
    } (** Logical OR *)
  | BinBitAnd of {
      loc: Loc.t (** Location *)
    } (** Bitwise AND *)
  | BinBitOr of {
      loc: Loc.t (** Location *)
    } (** Bitwise OR *)
  | BinBitXor of {
      loc: Loc.t (** Location *)
    } (** Bitwise XOR *)
(**
  Binary operators

  @since 1.0
*)

type patt = private
  | PattGround of {
      loc: Loc.t (** Location *)
    } (** Ground *)
  | PattVar of {
      loc: Loc.t; (** Location *)
      id:  Sym.t  (** Identifier *)
    } (** Variable *)
(**
  Patterns

  @since 1.0
*)

type rune =
  | RuneLit of {
      loc:   Loc.t;  (** Location *)
      value: Uchar.t (** Codepoint *)
    } (** A unicode rune *)
  | RuneEscape of {
      loc: Loc.t;    (** Location *)
      lexeme: string (** Lexeme *)
    } (** A unicode escape sequence *)
(**
  Runes

  @since 1.0
*)

type str =
  | StringLit of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** A string literal *)
  | StringEscape of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** A unicode escape sequence. *)
(**
  Strings

  @since 1.0
*)

type expr = private
  | ExprBool of {
      loc:   Loc.t; (** Location *)
      value: bool   (** Value *)
    } (** Boolean literal *)
  | ExprInt of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** Integer literal *)
  | ExprLong of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** Long literal *)
  | ExprFloat of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** Float literal *)
  | ExprDouble of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** Double literal *)
  | ExprRune of {
      loc:   Loc.t; (** Location *)
      value: rune   (** Value *)
    } (** Rune literal *)
  | ExprString of {
      loc:   Loc.t;        (** Location *)
      value: str list list (** UTF-8 encoded value *)
    } (** String literal *)
  | ExprIdent of {
      loc: Loc.t; (** Location *)
      id:  Sym.t  (** Identifier *)
    } (** Identifier *)
  | ExprUnOp of {
      loc:     Loc.t; (** Location *)
      op:      un;    (** Operator *)
      operand: expr   (** Operand *)
    } (** Unary Operation *)
  | ExprBinOp of {
      loc: Loc.t; (** Location *)
      op:  bin;   (** Operator *)
      lhs: expr;  (** Left-hand operand *)
      rhs: expr   (** Right-hand operand *)
    } (** Binary Operation *)
  | ExprLet of {
      loc:     Loc.t;   (** Location *)
      binding: binding; (** Binding *)
      scope:   expr     (** Scope *)
    } (** Let Binding *)
(**
  Expressions

  @since 1.0
*)

and binding = private
  | ValueBinding of {
      loc:   Loc.t;     (** Location *)
      patt:  patt;      (** Pattern *)
      ty:    ty option; (** Optional type annotation *)
      value: expr       (** Value expression *)
    } (** Value Binding *)
(**
  Bindings

  @since 1.0
*)

type top = private
  | TopLet of {
      loc:     Loc.t;  (** Location *)
      binding: binding (** Binding *)
    } (** Let Binding *)
  | TopVal of {
      loc:     Loc.t;  (** Location *)
      binding: binding (** Binding *)
    } (** Value Binding *)
(**
  Top-level bindings

  @since 1.0
*)

type name = private
  | Name of {
      loc: Loc.t; (** Location *)
      id:  Sym.t  (** Identifier *)
    } (** A name *)
(**
  Import names

  @since 1.0
*)

type proto = private
  | Proto of {
      loc:   Loc.t; (** Location *)
      proto: string (** Protocol *)
    } (** Protocol *)
(**
  Protocols

  @since 1.0
*)

type host = private
  | Host of {
      loc:  Loc.t; (** Location *)
      host: string (** Hostname *)
    } (** Host *)
(**
  Hosts

  @since 1.0
*)

type hostpath = private
  | HostPath of {
      loc:  Loc.t;      (** Location *)
      segs: string list (** Segments *)
    } (** Host Path *)
(**
  Host paths

  @since 1.0
*)

type version = private
  | Version of {
      loc:     Loc.t; (** Location *)
      version: string (** Version *)
    } (** Version *)
(**
  Versions

  @since 1.0
*)

type src = private
  | Current of {
      loc: Loc.t (** Location *)
    } (** Current package *)
  | External of {
      loc:     Loc.t;           (** Location *)
      proto:   proto option;    (** Optional Protocol *)
      host:    host;            (** Host *)
      path:    hostpath option; (** Path *)
      version: version;         (** Major version *)
    } (** External package *)
(**
  Import sources

  @since 1.0
*)

type from = private
  | From of {
      loc: Loc.t; (** Location *)
      src: src   (** Source to import from *)
    } (** From clause *)
(**
  From clauses

  @since 1.0
*)

type pkgpath = private
  | PkgPath of {
      loc:  Loc.t;   (** Location *)
      path: str list (** Path *)
    } (** Package Path *)
(**
  Package paths

  @since 1.0
*)

type alias = private
  | Alias of {
      loc:   Loc.t;      (** Location *)
      pkg:   pkgpath;    (** Package to import *)
      alias: name option (** Optional local name *)
    } (** A package alias *)
(**
  Package aliases

  @since 1.0
*)

type pkgs = private
  | Packages of {
      loc:  Loc.t;     (** Location *)
      pkgs: alias list (** The packages to import *)
    } (** A package list *)
(**
  Package lists

  @since 1.0
*)

type import = private
  | Import of {
      loc:  Loc.t;       (** Location *)
      from: from option; (** Optional import source *)
      pkgs: pkgs         (** The packages to import *)
    } (** An import statement *)
(**
  Import statements

  @since 1.0
*)

type pkg = private
  | Package of {
      loc: Loc.t; (** Location *)
      id:  name   (** Name of the package *)
    } (** A package statement *)
(**
  Package statements

  @since 1.0
*)

type file = private
  | File of {
      pkg:     pkg;         (** Package statement *)
      imports: import list; (** Import statements *)
      tops:    top list     (** Top-level expressions *)
    } (** A source file *)
(**
  Source files

  @since 1.0
*)

(** {2 Constructors} *)

(** {3 Types} *)

val ty_constr : Loc.t -> Sym.t -> ty
(**
  Construct a type constructor for a named type.

  @param loc The location of the type constructor
  @param id The type of the type constructor
  @return A type constructor
  @since 1.0
*)

(** {3 Operators} *)

(** {4 Unary} *)

val un_neg : Loc.t -> un
(**
  Construct a unary negation operator.

  @param loc The location of the operator
  @return A unary negation operator
  @since 1.0
*)

val un_log_not : Loc.t -> un
(**
  Construct a unary logical NOT operator.

  @param loc The location of the operator
  @return A unary logical NOT operator
  @since 1.0
*)

val un_bit_not : Loc.t -> un
(**
  Construct a unary bitwise NOT operator.

  @param loc The location of the operator
  @return A unary bitwise NOT operator
  @since 1.0
*)

(** {4 Binary} *)

val bin_struct_eq : Loc.t -> bin
(**
  Construct a binary structural equality operator.

  @param loc The location of the operator
  @return A binary structural equality operator
  @since 1.0
*)

val bin_struct_neq : Loc.t -> bin
(**
  Construct a binary structural inequality operator.

  @param loc The location of the operator
  @return A binary structural inequality operator
  @since 1.0
*)

val bin_phys_eq : Loc.t -> bin
(**
  Construct a binary physical equality operator.

  @param loc The location of the operator
  @return A binary physical equality operator
  @since 1.0
*)

val bin_phys_neq : Loc.t -> bin
(**
  Construct a binary physical inequality operator.

  @param loc The location of the operator
  @return A binary physical inequality operator
  @since 1.0
*)

val bin_lt : Loc.t -> bin
(**
  Construct a binary less than operator.

  @param loc The location of the operator
  @return A binary less than operator
  @since 1.0
*)

val bin_lte : Loc.t -> bin
(**
  Construct a binary less or equal than operator.

  @param loc The location of the operator
  @return A binary less or equal than operator
  @since 1.0
*)

val bin_gt : Loc.t -> bin
(**
  Construct a binary greater than operator.

  @param loc The location of the operator
  @return A binary greater than operator
  @since 1.0
*)

val bin_gte : Loc.t -> bin
(**
  Construct a binary greater than or equal operator.

  @param loc The location of the operator
  @return A binary greater than or equal operator
  @since 1.0
*)

val bin_lsl : Loc.t -> bin
(**
  Construct a binary logical shift left operator.

  @param loc The location of the operator
  @return A binary logical shift left operator
  @since 1.0
*)

val bin_asr : Loc.t -> bin
(**
  Construct a binary logical shift right operator.

  @param loc The location of the operator
  @return A binary logical shift right operator
  @since 1.0
*)

val bin_asl : Loc.t -> bin
(**
  Construct a binary arithmetic shift left operator.

  @param loc The location of the operator
  @return A binary arithmetic shift left operator
  @since 1.0
*)

val bin_lsr : Loc.t -> bin
(**
  Construct a binary arithmetic shift right operator.

  @param loc The location of the operator
  @return A binary arithmetic shift right operator
  @since 1.0
*)

val bin_add : Loc.t -> bin
(**
  Construct a binary addition operator.

  @param loc The location of the operator
  @return A binary addition operator
  @since 1.0
*)

val bin_sub : Loc.t -> bin
(**
  Construct a binary subtraction operator.

  @param loc The location of the operator
  @return A binary subtraction operator
  @since 1.0
*)

val bin_mul : Loc.t -> bin
(**
  Construct a binary multiplication operator.

  @param loc The location of the operator
  @return A binary multiplication operator
  @since 1.0
*)

val bin_div : Loc.t -> bin
(**
  Construct a binary division operator.

  @param loc The location of the operator
  @return A binary division operator
  @since 1.0
*)

val bin_mod : Loc.t -> bin
(**
  Construct a binary modulus operator.

  @param loc The location of the operator
  @return A binary modulus operator
  @since 1.0
*)

val bin_exp : Loc.t -> bin
(**
  Construct a binary exponentiation operator.

  @param loc The location of the operator
  @return A binary exponentiation operator
  @since 1.0
*)

val bin_log_and : Loc.t -> bin
(**
  Construct a binary logical AND operator.

  @param loc The location of the operator
  @return A binary logical AND operator
  @since 1.0
*)

val bin_log_or : Loc.t -> bin
(**
  Construct a binary logical OR operator.

  @param loc The location of the operator
  @return A binary logical OR operator
  @since 1.0
*)

val bin_bit_and : Loc.t -> bin
(**
  Construct a binary bitwise AND operator.

  @param loc The location of the operator
  @return A binary bitwise AND operator
  @since 1.0
*)

val bin_bit_or : Loc.t -> bin
(**
  Construct a binary bitwise OR operator.

  @param loc The location of the operator
  @return A binary bitwise OR operator
  @since 1.0
*)

val bin_bit_xor : Loc.t -> bin
(**
  Construct a binary bitwise XOR operator.

  @param loc The location of the operator
  @return A binary bitwise XOR operator
  @since 1.0
*)

(** {3 Patterns} *)

val patt_ground : Loc.t -> patt
(**
  Construct a ground ([_]) pattern.

  @param loc The location of the pattern
  @return A ground pattern
  @since 1.0
*)

val patt_var : Loc.t -> Sym.t -> patt
(**
  Construct a variable pattern binding an identifier.

  @param loc The location of the pattern
  @param id The identifier to bind
  @return A variable pattern binding the identifier
  @since 1.0
*)

(** {3 Runes} *)

val rune_lit : Loc.t -> Uchar.t -> rune
(**
  Construct a rune literal from a value.

  @param loc The location of the literal
  @param value The UTF-8 character value
  @return A rune literal
  @since 1.0
*)

val rune_escape : Loc.t -> string -> rune
(**
  Construct a Unicode escape sequence rune from a lexeme.

  @param loc The location of the escape sequence
  @param lexeme The value lexeme
  @return An escaped rune literal
*)

(** {3 Strings} *)

val str_lit : Loc.t -> string -> str
(**
  Construct a string literal string segment from a lexeme.

  @param loc The location of the literal
  @param lexeme The value lexeme
  @return A string literal string segment
  @since 1.0
*)

val str_escape : Loc.t -> string -> str
(**
  Construct a Unicode escape sequence string segment.

  @param loc The location of the literal
  @param lexeme The value lexeme
  @return A Unicode escape sequence string segment
  @since 1.0
*)

(** {3 Expressions} *)

val expr_bool : Loc.t -> bool -> expr
(**
  Construct a boolean literal from a value.

  @param loc The location of the literal
  @param value The boolean value
  @return A boolean literal
  @since 1.0
*)

val expr_int : Loc.t -> string -> expr
(**
  Construct an integer literal from a lexeme.

  @param loc The location of the literal
  @param lexeme The value lexeme
  @return An integer literal
  @since 1.0
*)

val expr_long : Loc.t -> string -> expr
(**
  Construct a long literal from a lexeme.

  @param loc The location of the literal
  @param lexeme The value lexeme
  @return A long literal
  @since 1.0
*)

val expr_float : Loc.t -> string -> expr
(**
  Construct a float literal from a lexeme.

  @param loc The location of the literal
  @param lexeme The value lexeme
  @return A float literal
  @since 1.0
*)

val expr_double : Loc.t -> string -> expr
(**
  Construct a double literal from a lexeme.

  @param loc The location of the literal
  @param lexeme The value lexeme
  @return A double literal
  @since 1.0
*)

val expr_rune : Loc.t -> rune -> expr
(**
  Construct a rune literal from a value.

  @param loc The location of the literal
  @param value The rune value
  @return A rune literal
  @since 1.0
*)

val expr_string : Loc.t -> str list list -> expr
(**
  Construct a string literal from string segments.  The segments are organized
  as a list of lines, each of which is a list of segments.

  @param loc The location of the literal
  @param value The string segments
  @return A string literal
  @since 1.0
*)

val expr_ident : Loc.t -> Sym.t -> expr
(**
  Construct a rune literal from an identifier.

  @param loc The location of the literal
  @param id The identifier's symbol
  @return An identifier
  @since 1.0
*)

val expr_un_op : Loc.t -> un -> expr -> expr
(**
  Construct unary operation applying an operator to an operand.

  @param loc The location of the expression
  @param op The unary oerator
  @param operand The operand
  @return A unary operation expression
  @since 1.0
*)

val expr_bin_op : Loc.t -> bin -> expr -> expr -> expr
(**
  Construct unary operation applying an operator to a pair of operands.

  @param loc The location of the expression
  @param op The binary oerator
  @param lhs The left operand
  @param rhs The right operand
  @return A binary operation expression
  @since 1.0
*)

val expr_let : Loc.t -> binding -> expr -> expr
(**
  Construct a let binding in a scope.

  @param loc The location of the expression
  @param binding The let binding
  @param scope The scope of the binding
  @return A let binding
  @since 1.0
*)

(** {3 Bindings} *)

val value_binding : Loc.t -> patt -> ty option -> expr -> binding
(**
  Construct a binding that binds a value of a type to a pattern.

  @param loc The location of the binding
  @param patt The pattern to bind to
  @param ty The type of the binding
  @param expr The value to bind
  @return A value binding
  @since 1.0
*)

(** {3 Top-Level Bindings} *)

val top_let : Loc.t -> binding -> top
(**
  Construct a top-level let binding.

  @param loc The location of the top-level binding
  @param binding The binding
  @return A top-level let binding
  @since 1.0
*)

val top_val : Loc.t -> binding -> top
(**
  Construct a top-level value binding.

  @param loc The location of the top-level binding
  @param binding The binding
  @return A top-level value binding
  @since 1.0
*)

(** {3 Imports} *)

val name : Loc.t -> Sym.t -> name
(**
  Construct a name of an identifier.

  @param loc The location of the name
  @param id The name's identifier symbol
  @return A name
  @since 1.0
*)

val proto : Loc.t -> string -> proto
(**
  Construct a protocol specifier.

  @param loc The location of the protocol
  @param proto The protocol name
  @return A protocol specifier
  @since 1.0
*)

val host : Loc.t -> string -> host
(**
  Construct a host identifier.

  @param loc The location of the host
  @param host The host identifier
  @return A host identifier
  @since 1.0
*)

val hostpath : Loc.t -> string list -> hostpath
(**
  Construct a host path from a list of segments.

  @param loc The location of the path
  @param segs The list of segments
  @return A host path
  @since 1.0
*)

val version : Loc.t -> string -> version
(**
  Construct a major version.

  @param loc The location of the major version
  @param version The major version number
  @return A major version
  @since 1.0
*)

val src_current : Loc.t -> src
(**
  Construct a reference to the current source.

  @param loc The location of the source reference
  @return The reference to the current source
  @since 1.0
*)

val src_external : Loc.t -> proto option -> host -> hostpath option -> version -> src
(**
  Construct a reference to an external source.

  @param loc The location of the source reference
  @param proto The protocol specifier
  @param host The host identifier
  @param path The host path
  @param version The major version
  @return An external source reference
  @since 1.0
*)

val from : Loc.t -> src -> from
(**
  Construct a [from] clause.

  @param loc The location of the from clause
  @param src The source to import from
  @return A from clause
*)

val pkgpath : Loc.t -> str list -> pkgpath
(**
  Construct a package path.

  @param loc The location of the package path
  @param path The path segments
  @return A package path
  @since 1.0
*)

val alias : Loc.t -> pkgpath -> name option -> alias
(**
  Construct a package alias.

  @param loc The location of the alias
  @param pkg The package path
  @param alias The local alias of the package
  @return A package alias
  @since 1.0
*)

val pkgs : Loc.t -> alias list -> pkgs
(**
  Construct a list of package aliases.

  @param loc The location of the alias list
  @param pkgs The package aliases
  @return A list of package aliases
  @since 1.0
*)

val import : Loc.t -> from option -> pkgs -> import
(**
  Construct an [import] statement.

  @param loc The location of the import statement
  @param from The from clause containing the source reference
  @param pkgs The list of packages to import
  @return An import clause
  @since 1.0
*)

(** {3 Package Statement} *)

val pkg : Loc.t -> name -> pkg
(**
  Construct a [package] statement.

  @param loc The location of the package statement
  @param id The name of the package
  @return A package statement
  @since 1.0
*)

(** {3 Source Files} *)

val file : pkg -> import list -> top list -> file
(**
  Construct a source file.

  @param pkg The package statement
  @param imports The import statements
  @param tops The top-level bindings
  @return A source file
  @since 1.0
*)

(** {2 Operations} *)

(** {3 Locations} *)

val loc_ty : ty -> Loc.t
(**
  Get the location of a type.

  @param ty The type to get the location of
  @return The location of the type
  @since 1.0
*)

val loc_un : un -> Loc.t
(**
  Get the location of a unary operator.

  @param op The operator to get the location of
  @return The location of the operator
  @since 1.0
*)

val loc_bin : bin -> Loc.t
(**
  Get the location of a binary operator.

  @param op The operator to get the location of
  @return The location of the operator
  @since 1.0
*)

val loc_rune : rune -> Loc.t
(**
  Get the location of a rune literal.

  @param rune The literal to get the location of
  @return The location of the literal
  @since 1.0
*)

val loc_str : str -> Loc.t
(**
  Get the location of a string segment.

  @param string The segment to get the location of
  @return The location of the segment
  @since 1.0
*)

val loc_expr : expr -> Loc.t
(**
  Get the location of an expression.

  @param expr The expression to get the location of
  @return The location of the expression
  @since 1.0
*)

val loc_patt : patt -> Loc.t
(**
  Get the location of a pattern.

  @param patt The pattern to get the location of
  @return The location of the pattern
  @since 1.0
*)

val loc_binding : binding -> Loc.t
(**
  Get the location of a value binding.

  @param binding The binding to get the location of
  @return The location of the binding
  @since 1.0
*)

val loc_top : top -> Loc.t
(**
  Get the location of a top-level binding.

  @param top The binding to get the location of
  @return The location of the binding
  @since 1.0
*)

val loc_name : name -> Loc.t
(**
  Get the location of an import name.

  @param name The name to get the location of
  @return The location of the name
  @since 1.0
*)

val loc_proto : proto -> Loc.t
(**
  Get the location of a protocol specifier.

  @param proto The protocol to get the location of
  @return The location of the protocol
  @since 1.0
*)

val loc_host : host -> Loc.t
(**
  Get the location of a host identifier.

  @param host The host to get the location of
  @return The location of the host
  @since 1.0
*)

val loc_hostpath : hostpath -> Loc.t
(**
  Get the location of a host path.

  @param path The host path to get the location of
  @return The location of the host path
  @since 1.0
*)

val loc_version : version -> Loc.t
(**
  Get the location of a major version.

  @param version The version to get the location of
  @return The location of the version
  @since 1.0
*)

val loc_src : src -> Loc.t
(**
  Get the location of an import source reference.

  @param src The source reference to get the location of
  @return The location of the source reference
  @since 1.0
*)

val loc_from : from -> Loc.t
(**
  Get the location of a [from] clause.

  @param from The from clause to get the location of
  @return The location of the from clause
  @since 1.0
*)

val loc_pkgpath : pkgpath -> Loc.t
(**
  Get the location of a package path.

  @param path The path to get the location of
  @return The location of the path
  @since 1.0
*)

val loc_alias : alias -> Loc.t
(**
  Get the location of an alias clause.

  @param alias The alias clause to get the location of
  @return The location of the alias clause
  @since 1.0
*)

val loc_pkgs : pkgs -> Loc.t
(**
  Get the location of a alias list.

  @param pkgs The list to get the location of
  @return The location of the list
  @since 1.0
*)

val loc_import : import -> Loc.t
(**
  Get the location of an [import] statement.

  @param import The import statement to get the location of
  @return The location of the import statement
  @since 1.0
*)

val loc_pkg : pkg -> Loc.t
(**
  Get the location of a [package] statement.

  @param pkg The package statement to get the location of
  @return The location of the package statement
  @since 1.0
*)

(** {3 Type Equality} *)

val ty_equal : ty -> ty -> bool
(**
  Test if two types are equal.  This is not a "deep equal" as it does not
  resolve type aliases.

  @param ty The first type
  @param ty' The second type
  @return [true] if the types are equal, [false] otherwise
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

val pp_un : formatter -> un -> unit
(**
  Pretty-print a unary operator to a formatter.

  @param fmt The formatter to print to
  @param op The operator to print
  @since 1.0
*)

val pp_bin : formatter -> bin -> unit
(**
  Pretty-print a binary operator to a formatter.

  @param fmt The formatter to print to
  @param op The operator to print
  @since 1.0
*)

val pp_rune : formatter -> rune -> unit
(**
  Pretty-print a rune literal to a formatter.

  @param fmt The formatter to print to
  @param rune The literal to print
  @since 1.0
*)

val pp_str : formatter -> str -> unit
(**
  Pretty-print a string segment to a formatter.

  @param fmt The formatter to print to
  @param str The segment to print
  @since 1.0
*)

val pp_expr : formatter -> expr -> unit
(**
  Pretty-print an expression to a formatter.

  @param fmt The formatter to print to
  @param expr The expression to print
  @since 1.0
*)

val pp_patt : formatter -> patt -> unit
(**
  Pretty-print a pattern to a formatter.

  @param fmt The formatter to print to
  @param patt The pattern to print
  @since 1.0
*)

val pp_binding : formatter -> binding -> unit
(**
  Pretty-print a value binding to a formatter.

  @param fmt The formatter to print to
  @param binding The binding to print
  @since 1.0
*)

val pp_top : formatter -> top -> unit
(**
  Pretty-print a top-level binding to a formatter.

  @param fmt The formatter to print to
  @param top The binding to print
  @since 1.0
*)

val pp_name : formatter -> name -> unit
(**
  Pretty-print an import name to a formatter.

  @param fmt The formatter to print to
  @param name The name to print
  @since 1.0
*)

val pp_proto : formatter -> proto -> unit
(**
  Pretty-print a protocol specifier to a formatter.

  @param fmt The formatter to print to
  @param proto The protocol to print
  @since 1.0
*)

val pp_host : formatter -> host -> unit
(**
  Pretty-print a host identifier to a formatter.

  @param fmt The formatter to print to
  @param host The host to print
  @since 1.0
*)

val pp_hostpath : formatter -> hostpath -> unit
(**
  Pretty-print a host path to a formatter.

  @param fmt The formatter to print to
  @param path The path to print
  @since 1.0
*)

val pp_version : formatter -> version -> unit
(**
  Pretty-print a major version to a formatter.

  @param fmt The formatter to print to
  @param version The version to print
  @since 1.0
*)

val pp_src : formatter -> src -> unit
(**
  Pretty-print an input source reference to a formatter.

  @param fmt The formatter to print to
  @param src The input source to print
  @since 1.0
*)

val pp_from : formatter -> from -> unit
(**
  Pretty-print a [from] clause to a formatter.

  @param fmt The formatter to print to
  @param from The from clause to print
  @since 1.0
*)

val pp_pkgpath : formatter -> pkgpath -> unit
(**
  Pretty-print a package path to a formatter.

  @param fmt The formatter to print to
  @param path The path to print
  @since 1.0
*)

val pp_alias : formatter -> alias -> unit
(**
  Pretty-print an import alias to a formatter.

  @param fmt The formatter to print to
  @param alias The alias to print
  @since 1.0
*)

val pp_pkgs : formatter -> pkgs -> unit
(**
  Pretty-print a package alias list to a formatter.

  @param fmt The formatter to print to
  @param pkg The package list to print
  @since 1.0
*)

val pp_import : formatter -> import -> unit
(**
  Pretty-print an [import] statement to a formatter.

  @param fmt The formatter to print to
  @param import The import statement to print
  @since 1.0
*)

val pp_pkg : formatter -> pkg -> unit
(**
  Pretty-print a [package] statement to a formatter.

  @param fmt The formatter to print to
  @param pkg The package statement to print
  @since 1.0
*)

val pp_file : formatter -> file -> unit
(**
  Pretty-print a file to a formatter.

  @param fmt The formatter to print to
  @param f The file to print
  @since 1.0
*)
