(** {1 Abstract Syntax Tests} *)

open OUnit2

open Common

(** {2 Test Suite} *)

val suite : test
(** [suite] is the test suite. *)

(**
 * {2 Fixtures}
 *
 * Generate fresh, unique values for testing.  All fixtures use
 * {!CommonTest.LocTest.gen} to generate any locations and take a symbol
 * sequence to use for generating fresh names.  If the symbol sequence is not
 * given, a new sequence is created.  The sequence is also passed down to any
 * lower-level fixtures generated as part of a higher-level fixture.
 *)

(** {3 Types} *)

val fresh_ty_constr : ?loc:Loc.t -> ?id:Sym.t -> unit -> Syntax.ty
(** [fresh_ty_constr ?loc ?id ()] constructs a fresh type constructor at
    location [loc] with the constructor [id]. *)

(** {3 Operators} *)

(** {4 Unary} *)

val fresh_un_neg : ?loc:Loc.t -> unit -> Syntax.un
(** [fresh_un_neg ?loc ()] constructs a fresh unary negation operator at
    location [loc]. *)

val fresh_un_log_not : ?loc:Loc.t -> unit -> Syntax.un
(** [fresh_un_log_not ?loc ()] constructs a fresh unary logical NOT operator
    at location [loc]. *)

val fresh_un_bit_not : ?loc:Loc.t -> unit -> Syntax.un
(** [fresh_un_bit_not ?loc ()] constructs a fresh unary bitwise NOT operator
    at location [loc]. *)

(** {4 Binary} *)

val fresh_bin_struct_eq : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_struct_eq ?loc ()] constructs a fresh binary structural equality
    operator at location [loc]. *)

val fresh_bin_struct_neq : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_struct_neq ?loc ()] constructs a fresh binary structural
    inequality operator at location [loc]. *)

val fresh_bin_phys_eq : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_phys_eq ?loc ()] constructs a fresh binary physical equality
    operator at location [loc]. *)

val fresh_bin_phys_neq : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_phys_neq ?loc ()] constructs a fresh binary physical inequality
    operator at location [loc]. *)

val fresh_bin_lt : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_lt ?loc ()] constructs a fresh binary less than operator at
    location [loc]. *)

val fresh_bin_lte : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_lte ?loc ()] constructs a fresh binary less than or equal
    operator at location [loc]. *)

val fresh_bin_gt : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_gt ?loc ()] constructs a fresh binary greater than operator at
    location [loc]. *)

val fresh_bin_gte : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_gte ?loc ()] constructs a fresh binary greater than or equal
    operator at location [loc]. *)

val fresh_bin_add : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_add ?loc ()] constructs a fresh binary addition operator at
    location [loc]. *)

val fresh_bin_sub : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_sub ?loc ()] constructs a fresh binary subtraction operator at
    location [loc]. *)

val fresh_bin_mul : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_mul ?loc ()] constructs a fresh binary multiplication operator at
    location [loc]. *)

val fresh_bin_div : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_div ?loc ()] constructs a fresh binary division operator at
    location [loc]. *)

val fresh_bin_mod : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_mod ?loc ()] constructs a fresh binary modulus operator at
    location [loc]. *)

val fresh_bin_exp : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_exp ?loc ()] constructs a fresh binary exponentiation operator at
    location [loc]. *)

val fresh_bin_log_and : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_log_and ?loc ()] constructs a fresh binary logical AND operator
    at location [loc]. *)

val fresh_bin_log_or : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_log_or ?loc ()] constructs a fresh binary logical OR operator at
    location [loc]. *)

val fresh_bin_bit_and : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_bit_and ?loc ()] constructs a fresh binary bitwise AND operator
    at location [loc]. *)

val fresh_bin_bit_or : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_bit_or ?loc ()] constructs a fresh binary bitwise OR operator at
    location [loc]. *)

val fresh_bin_bit_xor : ?loc:Loc.t -> unit -> Syntax.bin
(** [fresh_bin_bit_xor ?loc ()] constructs a fresh binary bitwise XOR operator
    at location [loc]. *)

(** {3 Patterns} *)

val fresh_patt_ground : ?loc:Loc.t -> unit -> Syntax.patt
(** [fresh_patt_ground ?loc ()] constructs a fresh ground pattern at location
    [loc]. *)

val fresh_patt_var : ?loc:Loc.t -> ?id:Sym.t -> unit -> Syntax.patt
(** [fresh_patt_var ?loc ?id ()] constructs a fresh identifier pattern at
    location [loc]. *)

(** {3 Runes} *)

val fresh_rune_lit : ?loc:Loc.t -> ?value:char -> unit -> Syntax.rune
(** [fresh_rune_lit ?loc ?value ()] constructs a fresh rune literal at location
    [loc] with value [value]. *)

val fresh_rune_escape : ?loc:Loc.t -> ?value:int -> unit -> Syntax.rune
(** [fresh_rune_escape ?loc ?value ()] constructs a fresh rune unicode escape
    sequence at location [loc] for the codepoint [value]. *)

(** {3 Strings} *)

val fresh_str_lit : ?loc:Loc.t -> ?value:string -> unit -> Syntax.str
(** [fresh_str_lit ?loc ?value ()] constructs a fresh string literal segment
    at location [loc] with lexeme [value]. *)

val fresh_str_escape : ?loc:Loc.t -> ?value:int -> unit -> Syntax.str
(** [fresh_str_escape ?loc ?value ()] constructs a fresh string unicode escape
    sequence segment at location [loc] for the codepoint [value]. *)

(** {3 Expressions} *)

val fresh_expr_bool : ?loc:Loc.t -> ?value:bool -> unit -> Syntax.expr
(** [fresh_expr_bool ?loc ?value ()] constructs a fresh boolean expression at
    location [loc] with value [value]. *)

val fresh_expr_int : ?loc:Loc.t -> ?value:int32 -> unit -> Syntax.expr
(** [fresh_expr_int ?loc ?value ()] constructs a fresh integer expression at
    location [loc] with value [value]. *)

val fresh_expr_long : ?loc:Loc.t -> ?value:int64 -> unit -> Syntax.expr
(** [fresh_expr_long ?loc ?value ()] constructs a fresh long expression at
    location [loc] with value [value]. *)

val fresh_expr_float : ?loc:Loc.t -> ?value:float -> unit -> Syntax.expr
(** [fresh_expr_float ?loc ?value ()] constructs a fresh float expression at
    location [loc] with value [value]. *)

val fresh_expr_double : ?loc:Loc.t -> ?value:float -> unit -> Syntax.expr
(** [fresh_expr_double ?loc ?value ()] constructs a fresh double expression at
    location [loc] with value [value]. *)

val fresh_expr_rune : ?loc:Loc.t -> ?value:Syntax.rune -> unit -> Syntax.expr
(** [fresh_expr_rune ?loc ?value ()] constructs a fresh rune expression at
    location [loc] with value [value]. *)

val fresh_expr_string : ?loc:Loc.t -> ?value:(Syntax.str list) -> unit -> Syntax.expr
(** [fresh_expr_string ?loc ?value ()] constructs a fresh string expression
    at location [loc] with value [value]. *)

val fresh_expr_ident : ?loc:Loc.t -> ?seq:Sym.seq -> ?id:string -> unit -> Syntax.expr
(** [fresh_expr_ident ?loc ?seq ?id ()] constructs a fresh identifier expression
    at location [loc]. *)

val fresh_expr_un_op : ?loc:Loc.t -> ?op:Syntax.un -> ?operand:Syntax.expr -> unit -> Syntax.expr
(** [fresh_expr_un_op ?loc ?op ?operand ()] constructs a fresh unary operator
    expression at location [loc] applying the operator [op] to the operand
    [operand]. *)

val fresh_expr_bin_op : ?loc:Loc.t -> ?op:Syntax.bin -> ?lhs:Syntax.expr -> ?rhs:Syntax.expr -> unit -> Syntax.expr
(** [fresh_expr_bin_op ?loc ?op ?lhs ?rhs ()] constructs a fresh binary operator
    expression at location [loc] applying the operator [op] to the operands
    [lhs] and [rhs]. *)

val fresh_expr_let : ?loc:Loc.t -> ?binding:Syntax.binding -> ?scope:Syntax.expr -> unit -> Syntax.expr
(** [fresh_expr_bin_op ?loc ?binding ?scope ()] constructs a fresh local let
    binding at location [loc] binding [binding] in the scope of [scope]. *)

(** {3 Bindings} *)

val fresh_value_binding : ?loc:Loc.t -> ?patt:Syntax.patt -> ?explicit:bool -> ?ty:Syntax.ty -> ?value:Syntax.expr -> unit -> Syntax.binding
(** [fresh_value_binding ?loc ?patt ?explicit ?ty ?value ()] constructs a fresh
    value binding at location [loc] binding the value [value] of type [ty] to
    the pattern [patt].  If [explicit] is [true], a type annotation is
    generated.  If not provided, [explicit] defaults to [false]. *)

(** {3 Top-Level Expressions} *)

val fresh_top_let : ?loc:Loc.t -> ?binding:Syntax.binding -> unit -> Syntax.top
(** [fresh_top_let ?loc ?binding ()] constructs a fresh top-level let binding at
    location [loc] with the binding [binding]. *)

val fresh_top_val : ?loc:Loc.t -> ?binding:Syntax.binding -> unit -> Syntax.top
(** [fresh_top_val ?loc ?binding ()] constructs a fresh top-level val binding at
    location [loc] with the binding [binding]. *)

(** {3 Imports} *)

val fresh_name : ?loc:Loc.t -> ?seq:Sym.seq -> ?id:string -> unit -> Syntax.name
(** [fresh_name ?loc ?seq ?id ()] constructs a fresh name at location [loc]
    using [id] as the name. *)

val fresh_src : ?loc:Loc.t -> ?name:Syntax.name -> unit -> Syntax.src
(** [fresh_srq ?loc ?name ()] constructs a fresh import source at location [loc]
    using [name] as the source's name. *)

val fresh_from : ?loc:Loc.t -> ?src:Syntax.src -> unit -> Syntax.from
(** [fresh_from ?loc ?src ()] generates a fresh from clause at location [loc]
    using [src] as the source name. *)

val fresh_alias : ?loc:Loc.t -> ?pkg:Syntax.name -> ?alias:bool -> ?local:Syntax.name -> unit -> Syntax.alias
(** [fresh_alias ?loc ?pkg ?local ()] generates a fresh alias clause at location
    location [loc] using [pkg] as the package name.  If [alias] is [true],
    [local] is used as the local alias.  If not provided, [alias] defaults to
    [false]. *)

val fresh_pkgs : ?loc:Loc.t -> ?aliases:(Syntax.alias list) -> unit -> Syntax.pkgs
(** [fresh_pkgs ?loc ?aliases ()] generates a fresh list of packages at location
    [loc] with the list of aliases [aliases].  If not provided, aliases defaults
    to an empty list. *)

val fresh_import : ?loc:Loc.t -> ?stdlib:bool -> ?from:Syntax.from -> ?pkgs:Syntax.pkgs -> unit -> Syntax.import
(** [fresh_import ?loc ?stdlib ?from ?pkgs ()] generates a fresh import
    statement at location [loc] import the packages [pkgs] from [from].  If
    [stdlib] is [false], then the from clause is included.  If not provided,
    [stdlib] defaults to [true]. *)

val fresh_pkg : ?loc:Loc.t -> ?id:Syntax.name -> unit -> Syntax.pkg
(** [fresh_pkg ?loc ?id ()] generates a fresh package clause at location [loc]
    using [id] as the package name. *)

(** {2 Utilities} *)

(**
 * {3 Location Stripping}
 *
 * Replaces all location information with {!CommonTest.LocTest.dummy}.  Useful
 * for normalizing parsed syntax with hand-constructed syntax in tests.
 *)

val deloc_ty : Syntax.ty -> Syntax.ty
(** [deloc_ty ty] strips location information from the type [ty]. *)

val deloc_un : Syntax.un -> Syntax.un
(** [deloc_un op] strips location information from the unary operator [op]. *)

val deloc_bin : Syntax.bin -> Syntax.bin
(** [deloc_bin op] strips location information from the binary operator [op]. *)

val deloc_expr : Syntax.expr -> Syntax.expr
(** [deloc_expr expr] strips location information from the expression [expr]. *)

val deloc_patt : Syntax.patt -> Syntax.patt
(** [deloc_patt patt] strips location information from the pattern [patt]. *)

val deloc_binding : Syntax.binding -> Syntax.binding
(** [deloc_binding binding] strips location information from the binding
    [binding]. *)

val deloc_top : Syntax.top -> Syntax.top
(** [deloc_top top] strips location information from the top-level expression
    [top]. *)

val deloc_name : Syntax.name -> Syntax.name
(** [deloc_name name] strips location information from the name [name]. *)

val deloc_src : Syntax.src -> Syntax.src
(** [deloc_src src] strips location information from the import source [src]. *)

val deloc_from : Syntax.from -> Syntax.from
(** [deloc_from from] strips location information from the from clause [from]. *)

val deloc_alias : Syntax.alias -> Syntax.alias
(** [deloc_alias alias] strips location information from the package alias
    [alias]. *)

val deloc_pkgs : Syntax.pkgs -> Syntax.pkgs
(** [deloc_pkgs pkgs] strips location information from the package list [pkgs]. *)

val deloc_import : Syntax.import -> Syntax.import
(** [deloc_import import] strips location information from the import statement
    [import]. *)

val deloc_pkg : Syntax.pkg -> Syntax.pkg
(** [deloc_pkg pkg] strips location information from the package statement
    [pkg]. *)

val deloc_file : Syntax.file -> Syntax.file
(** [deloc_file file] strips location information from the source file [file]. *)

(** {2 Assertions} *)

(**
 * {3 Equality}
 *
 * All equality assertions include location equality.  To normalize location
 * information, use the [deloc_...] family of functions.
 *
 * The test context passed in is passed down to all internal assertions.
 *)

val assert_ty_equal : ctxt:test_ctxt -> Syntax.ty -> Syntax.ty -> unit
(** [assert_ty_equal ~ctxt expected actual] asserts that the type [actual] is
    equal to the type [expected]. *)

val assert_un_equal : ctxt:test_ctxt -> Syntax.un -> Syntax.un -> unit
(** [assert_un_equal ~ctxt expected actual] asserts that the unary operator
    [actual] is equal to the unary operator [expected]. *)

val assert_bin_equal : ctxt:test_ctxt -> Syntax.bin -> Syntax.bin -> unit
(** [assert_bin_equal ~ctxt expected actual] asserts that the binary operator
    [actual] is equal to the binary operator [expected]. *)

val assert_rune_equal : ctxt:test_ctxt -> Syntax.rune -> Syntax.rune -> unit
(** [assert_rune_equal ~ctxt expected actual] asserts that the rune [actual] is
    equal to the rune operator [expected]. *)

val assert_str_equal : ctxt:test_ctxt -> Syntax.str -> Syntax.str -> unit
(** [assert_str_equal ~ctxt expected actual] asserts that the string segment
    [actual] is equal to the string segment operator [expected]. *)

val assert_expr_equal : ctxt:test_ctxt -> Syntax.expr -> Syntax.expr -> unit
(** [assert_expr_equal ~ctxt expected actual] asserts that the expression
    [actual] is equal to the expression [expected]. *)

val assert_patt_equal : ctxt:test_ctxt -> Syntax.patt -> Syntax.patt -> unit
(** [assert_patt_equal ~ctxt expected actual] asserts that the pattern [actual]
    is equal to the pattern [expected]. *)

val assert_binding_equal : ctxt:test_ctxt -> Syntax.binding -> Syntax.binding -> unit
(** [assert_binding_equal ~ctxt expected actual] asserts that the binding
    [actual] is equal to the binding [expected]. *)

val assert_top_equal : ctxt:test_ctxt -> Syntax.top -> Syntax.top -> unit
(** [assert_top_equal ~ctxt expected actual] asserts that the top-level
    expression [actual] is equal to the top-level expression [expected]. *)

val assert_name_equal : ctxt:test_ctxt -> Syntax.name -> Syntax.name -> unit
(** [assert_name_equal ~ctxt expected actual] asserts that the name [actual] is
    equal to the name [expected]. *)

val assert_src_equal : ctxt:test_ctxt -> Syntax.src -> Syntax.src -> unit
(** [assert_src_equal ~ctxt expected actual] asserts that the import source
    [actual] is equal to the import source [expected]. *)

val assert_from_equal : ctxt:test_ctxt -> Syntax.from -> Syntax.from -> unit
(** [assert_from_equal ~ctxt expected actual] asserts that the from clause
    [actual] is equal to the from clause [expected]. *)

val assert_alias_equal : ctxt:test_ctxt -> Syntax.alias -> Syntax.alias -> unit
(** [assert_alias_equal ~ctxt expected actual] asserts that the package alias
    [actual] is equal to the package alias [expected]. *)

val assert_pkgs_equal : ctxt:test_ctxt -> Syntax.pkgs -> Syntax.pkgs -> unit
(** [assert_pkgs_equal ~ctxt expected actual] asserts that the package list
    [actual] is equal to the package list [expected].  This comparison is
    order-dependent. *)

val assert_import_equal : ctxt:test_ctxt -> Syntax.import -> Syntax.import -> unit
(** [assert_import_equal ~ctxt expected actual] asserts that the import
    statement [actual] is equal to the import statement [expected]. *)

val assert_pkg_equal : ctxt:test_ctxt -> Syntax.pkg -> Syntax.pkg -> unit
(** [assert_pkg_equal ~ctxt expected actual] asserts that the package statement
    [actual] is equal to the package statement [expected]. *)

val assert_file_equal : ctxt:test_ctxt -> Syntax.file -> Syntax.file -> unit
(** [assert_file_equal ~ctxt expected actual] asserts that the source file
    [actual] is equal to the source file [expected]. *)
