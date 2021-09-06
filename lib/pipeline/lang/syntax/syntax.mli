(** {1 Abstract Syntax} *)

open Format

open Common

(** {2 Syntax} *)

type ty = private
  | TyConstr of {
      loc: Loc.t; (** Location *)
      id:  Sym.t  (** Constructor *)
    } (** Type Constructor *)
(** Types *)

type rune =
  | RuneLit of {
      loc:   Loc.t;  (** Location *)
      value: Uchar.t (** Codepoint *)
    } (** A unicode rune *)
  | RuneEscape of {
      loc: Loc.t;    (** Location *)
      lexeme: string (** Lexeme *)
    } (** A unicode escape sequence *)
(** Runes *)

type str =
  | StringLit of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** A string literal *)
  | StringEscape of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** A unicode escape sequence. *)
(** Strings *)

type un =
  | UnNeg of {
      loc: Loc.t (** Location *)
    } (** Negation *)
(** Unary Operators *)

type bin =
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
(** Binary Operators *)

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
      loc:   Loc.t;   (** Location *)
      value: str list (** UTF-8 encoded value *)
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
(** Expressions *)

type patt = private
  | PattGround of {
      loc: Loc.t (** Location *)
    } (** Ground *)
  | PattVar of {
      loc: Loc.t; (** Location *)
      id:  Sym.t  (** Identifier *)
    } (** Variable *)
(** Patterns *)

type binding = private
  | ValueBinding of {
      loc:   Loc.t;     (** Location *)
      patt:  patt;      (** Pattern *)
      ty:    ty option; (** Optional type annotation *)
      value: expr       (** Value expression *)
    } (** Value Binding *)
(** Bindings *)

type top = private
  | TopLet of {
      loc:     Loc.t;  (** Location *)
      binding: binding (** Binding *)
    } (** Let Binding *)
  | TopVal of {
      loc:     Loc.t;  (** Location *)
      binding: binding (** Binding *)
    } (** Value Binding *)
(** Top-Level Bindings *)

type name = private
  | Name of {
      loc: Loc.t; (** Location *)
      id:  Sym.t  (** Identifier *)
    } (** A name *)
(** Import Names *)

type src = private
  | Source of {
      loc:  Loc.t; (** Location *)
      name: name   (** Name of the source *)
    } (** An import source *)
(** Import Sources *)

type from = private
  | From of {
      loc: Loc.t; (** Location *)
      src: src   (** Source to import from *)
    } (** From clause *)
(** From Clauses *)

type alias = private
  | Alias of {
      loc:   Loc.t;      (** Location *)
      pkg:   name;       (** Package to import *)
      alias: name option (** Optional local name *)
    } (** A package alias *)
(** Package Aliases *)

type pkgs = private
  | Packages of {
      loc:  Loc.t;     (** Location *)
      pkgs: alias list (** The packages to import *)
    } (** A package list *)
(** Package List *)

type import = private
  | Import of {
      loc:  Loc.t;       (** Location *)
      from: from option; (** Optional import source *)
      pkgs: pkgs         (** The packages to import *)
    } (** An import statement *)
(** Import Statements *)

type pkg = private
  | Package of {
      loc: Loc.t; (** Location *)
      id:  name   (** Name of the package *)
    } (** A package statement *)
(** Package Statements *)

type file = private
  | File of {
      pkg:     pkg;         (** Package statement *)
      imports: import list; (** Import statements *)
      tops:    top list     (** Top-level expressions *)
    } (** A source file *)
(** Source Files *)

(** {2 Constructors} *)

(** {3 Types} *)

val ty_constr : Loc.t -> Sym.t -> ty
(** [ty_constr loc id] constructs a type constructor at location [loc] for the
    type [id]. *)

(** {3 Runes} *)

val rune_lit : Loc.t -> Uchar.t -> rune
(** [rune_lit loc value] constructs a unicode rune at location [loc] with the
    value [value]. *)

val rune_escape : Loc.t -> string -> rune
(** [rune_escape loc lexeme] constructs a unicode escape sequence rune at
    location [loc] with lexeme [lexeme]. *)

(** {3 Strings} *)

val str_lit : Loc.t -> string -> str
(** [str_lit loc lexeme] constructs a string literal string segment at location
    [loc] with lexeme [lexeme]. *)

val str_escape : Loc.t -> string -> str
(** [str_escape loc lexeme] constructs a unicode escape sequence string segment
    at location [loc] with lexeme [lexeme]. *)

(** {3 Operators} *)

val un_neg : Loc.t -> un
(** [un_neg loc] constructs a unary negation operator at location [loc]. *)

val bin_add : Loc.t -> bin
(** [bin_add loc] constructs a binary addition operator at location [loc]. *)

val bin_sub : Loc.t -> bin
(** [bin_sub loc] constructs a binary subtraction operator at location [loc]. *)

val bin_mul : Loc.t -> bin
(** [bin_mul loc] constructs a binary multiplication operator at location [loc]. *)

val bin_div : Loc.t -> bin
(** [bin_div loc] constructs a binary division operator at location [loc]. *)

val bin_mod : Loc.t -> bin
(** [bin_mod loc] constructs a binary modulus operator at location [loc]. *)

val bin_exp : Loc.t -> bin
(** [bin_exp loc] constructs a binary exponentiation operator at location [loc]. *)

(** {3 Expressions} *)

val expr_bool : Loc.t -> bool -> expr
(** [expr_bool loc value] constructs a boolean literal at location [loc] with
    value [value]. *)

val expr_int : Loc.t -> string -> expr
(** [expr_int loc lexeme] constructs an integer literal at location [loc] with
    lexeme [lexeme]. *)

val expr_long : Loc.t -> string -> expr
(** [expr_long loc lexeme] constructs a long literal at location [loc] with
    lexeme [lexeme]. *)

val expr_float : Loc.t -> string -> expr
(** [expr_float loc lexeme] constructs a float literal at location [loc] with
    lexeme [lexeme]. *)

val expr_double : Loc.t -> string -> expr
(** [expr_double loc lexeme] constructs a double literal at location [loc] with
    lexeme [lexeme]. *)

val expr_rune : Loc.t -> rune -> expr
(** [expr_rune loc value] constructs a rune literal at location [loc] with value
    [value]. *)

val expr_string : Loc.t -> str list -> expr
(** [expr_string loc value] constructs a string literal at location [loc] with
    value [value]. *)

val expr_ident : Loc.t -> Sym.t -> expr
(** [expr_ident loc id] constructs an identifier at location [loc] with
    identifier [id]. *)

val expr_un_op : Loc.t -> un -> expr -> expr
(** [expr_un_op loc op operand] constructs a unary operation at location [loc]
    applying the unary operator [op] to the operand [operand]. *)

val expr_bin_op : Loc.t -> bin -> expr -> expr -> expr
(** [expr_bin_op loc op lhs rhs] constructs a binary operation at location [loc]
    applying the binary operator [op] to the operands [lhs] and [rhs]. *)

(** {3 Patterns} *)

val patt_ground : Loc.t -> patt
(** [patt_ground loc] constructs a ground pattern at location [loc]. *)

val patt_var : Loc.t -> Sym.t -> patt
(** [patt_var loc id] constructs a variable pattern at location [loc] binding
    the identifier [id]. *)

(** {3 Bindings} *)

val value_binding : Loc.t -> patt -> ty option -> expr -> binding
(** [value_binding loc patt ty value] constructs a value binding at location
    [loc] binding the [value] of type [ty] to the pattern [pattern]. *)

(** {3 Top-Level Bindings} *)

val top_let : Loc.t -> binding -> top
(** [top_let loc binding] constructs a top-level let binding at location [loc]
    with binding [binding]. *)

val top_val : Loc.t -> binding -> top
(** [top_val loc binding] constructs a top-level value binding at location [loc]
    with binding [binding]. *)

(** {3 Imports} *)

val name : Loc.t -> Sym.t -> name
(** [name loc id] constructs a name at location [loc] of the identifier [id]. *)

val src : Loc.t -> name -> src
(** [src loc name] constructs a source reference at location [loc] with the name
    [name]. *)

val from : Loc.t -> src -> from
(** [from_clause loc src] constructs a [from] clause at location [loc] importing
    from the source [src]. *)

val alias : Loc.t -> name -> name option -> alias
(** [alias loc pkg alias] constructs a package alias at location [loc] importing
    the package [pkg] with the optional local alias [alias]. *)

val pkgs : Loc.t -> alias list -> pkgs
(** [pkgs loc pkgs] constructs a list of package aliases as location [loc]. *)

val import : Loc.t -> from option -> pkgs -> import
(** [import loc from pkgs] constructs an import statement at location [loc]
    importing the packages [pkgs] from the optional source [from]. *)

(** {3 Package Statement} *)

val pkg : Loc.t -> name -> pkg
(** [pkg loc id] constructs a package statement at location [loc] with the name
    [id]. *)

(** {3 Source Files} *)

val file : pkg -> import list -> top list -> file
(** [file pkg imports tops] constructs a source file where [pkg] is the package
    statement, [imports] is the list of import statements, and [tops] is the
    list of top-level statements. *)

(** {2 Operations} *)

(** {3 Locations} *)

val loc_ty : ty -> Loc.t
(** [loc_ty ty] returns the location of the type [ty]. *)

val loc_un : un -> Loc.t
(** [loc_un op] returns the location of the unary operator [op]. *)

val loc_bin : bin -> Loc.t
(** [loc_bin op] returns the location of the binary operator [op]. *)

val loc_expr : expr -> Loc.t
(** [loc_expr expr] returns the location of the expression [expr]. *)

val loc_patt : patt -> Loc.t
(** [loc_patt patt] returns the location of the pattern [patt]. *)

val loc_binding : binding -> Loc.t
(** [loc_binding binding] returns the location of the binding [binding]. *)

val loc_top : top -> Loc.t
(** [loc_top top] returns the location of the top-level expression [top]. *)

val loc_name : name -> Loc.t
(** [loc_name name] returns the location of the import name [name]. *)

val loc_src : src -> Loc.t
(** [loc_src src] returns the location of the import source [src]. *)

val loc_from : from -> Loc.t
(** [loc_from from] returns the location of the from clause [from]. *)

val loc_alias : alias -> Loc.t
(** [loc_alias alias] returns the location of the alias clause [alias]. *)

val loc_pkgs : pkgs -> Loc.t
(** [loc_pkgs pkgs] returns the location of the alias list [pkgs]. *)

val loc_import : import -> Loc.t
(** [loc_import import] returns the location of the import statement [import]. *)

val loc_pkg : pkg -> Loc.t
(** [loc_pkg pkg] returns the location of the package statement [pkg]. *)

(** {3 Type Equality} *)

val ty_equal : ty -> ty -> bool
(** [ty_equal ty ty'] tests if the type [ty] is equal to the type [ty']. *)

(** {3 Pretty Printing} *)

val pp_ty : formatter -> ty -> unit
(** [pp_ty fmt ty] pretty-prints the type [ty] to the formatter [fmt]. *)

val pp_un : formatter -> un -> unit
(** [pp_un fmt op] pretty-prints the unary operator [op] to the formatter [fmt]. *)

val pp_bin : formatter -> bin -> unit
(** [pp_bin fmt op] pretty-prints the binary operator [op] to the formatter
    [fmt]. *)

val pp_rune : formatter -> rune -> unit
(** [pp_rune fmt r] pretty-prints the rune [r] to the formatter [fmt]. *)

val pp_str : formatter -> str -> unit
(** [pp_str fmt str] pretty-prints the string segment [str] to the formatter
    [fmt]. *)

val pp_expr : formatter -> expr -> unit
(** [pp_expr fmt expr] pretty-prints the expression [expr] to the formatter
    [fmt]. *)

val pp_patt : formatter -> patt -> unit
(** [pp_patt fmt patt] pretty-prints the pattern [patt] to the formatter [fmt]. *)

val pp_binding : formatter -> binding -> unit
(** [pp_binding fmt binding] pretty-prints the binding [binding] to the
    formatter [fmt]. *)

val pp_top : formatter -> top -> unit
(** [pp_top fmt top] pretty-prints the top-level expression [top] to the
    formatter [fmt]. *)

val pp_name : formatter -> name -> unit
(** [pp_name fmt name] pretty-prints the import name [name] to the formatter
    [fmt]. *)

val pp_src : formatter -> src -> unit
(** [pp_src fmt src] pretty-prints the import source [src] to the formatter
    [fmt]. *)

val pp_from : formatter -> from -> unit
(** [pp_from fmt from] pretty-prints the from clause [from] to the formatter
    [fmt]. *)

val pp_alias : formatter -> alias -> unit
(** [pp_alias fmt alias] pretty-prints the import alias [alias] to the formatter
    [fmt]. *)

val pp_pkgs : formatter -> pkgs -> unit
(** [pp_pkgs fmt pkgs] pretty-prints the package list [pkgs] to the formatter
    [fmt]. *)

val pp_import : formatter -> import -> unit
(** [pp_import fmt import] pretty-prints the import statement [import] to the
    formatter [fmt]. *)

val pp_pkg : formatter -> pkg -> unit
(** [pp_pkg fmt pkg] pretty-prints the pkg statement [pkg] to the formatter
    [fmt]. *)

val pp_file : formatter -> file -> unit
(** [pp_file fmt f] pretty-prints the file [f] to the formatter [fmt]. *)
