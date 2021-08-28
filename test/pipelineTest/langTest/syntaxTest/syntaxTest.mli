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

val fresh_ty_constr : ?seq:Sym.seq -> ?id:string -> unit -> Syntax.ty
(** [fresh_ty_constr ?seq ?id ()] constructs a fresh type constructor.  If not
    provided, [id] defaults to {!Common.Prim.id_bool}. *)

val fresh_expr_bool : ?value:bool -> unit -> Syntax.expr
(** [fresh_expr_bool ?value ()] constructs a fresh boolean expression with value
    [value].  If not provided, [value] defaults to [true]. *)

val fresh_expr_int : ?value:int32 -> unit -> Syntax.expr
(** [fresh_expr_int ?value ()] constructs a fresh integer expression with value
    [value].  If not provided, [value] defaults to [42l]. *)

val fresh_expr_long : ?value:int64 -> unit -> Syntax.expr
(** [fresh_expr_long ?value ()] constructs a fresh long expression with value
    [value].  If not provided, [value] defaults to [42L]. *)

val fresh_expr_float : ?value:float -> unit -> Syntax.expr
(** [fresh_expr_float ?value ()] constructs a fresh float expression with value
    [value].  If not provided, [value] defaults to [4.2]. *)

val fresh_expr_double : ?value:float -> unit -> Syntax.expr
(** [fresh_expr_double ?value ()] constructs a fresh double expression with
    value [value].  If not provided, [value] defaults to [4.2]. *)

val fresh_expr_rune : ?value:char -> unit -> Syntax.expr
(** [fresh_expr_rune ?value ()] constructs a fresh rune expression with value
    [value].  If not provided, [value] defaults to ['a']. *)

val fresh_expr_string : ?value:string -> unit -> Syntax.expr
(** [fresh_expr_string ?value ()] constructs a fresh string expression with
    value [value].  If not provided, [value] defaults to ["foo bar"]. *)

val fresh_expr_ident : ?seq:Sym.seq -> ?id:string -> unit -> Syntax.expr
(** [fresh_expr_ident ?seq ?id ()] constructs a fresh identifier expression. *)

val fresh_patt_ground : unit -> Syntax.patt
(** [fresh_patt_ground ()] constructs a fresh ground pattern. *)

val fresh_patt_var : ?seq:Sym.seq -> ?id:string -> unit -> Syntax.patt
(** [fresh_patt_var ?seq ?id ()] constructs a fresh identifier pattern. *)

val fresh_value_binding : ?explicit:bool -> ?seq:Sym.seq -> ?id:string -> unit -> Syntax.binding
(** [fresh_value_binding ?explicit ?seq ?id ()] constructs a fresh value
    binding.  If [explicit] is true, a type annotation is generated.  If not
    provided, [explicit] defaults to [false]. *)

val fresh_name : ?seq:Sym.seq -> ?id:string -> unit -> Syntax.name
(** [fresh_name ?seq ?id ()] constructs a fresh name using [id] as the name.  If
     not provided, [id] defaults to [""]. *)

val fresh_src : ?seq:Sym.seq -> ?name:string -> unit -> Syntax.src
(** [fresh_srq ?seq ?name ()] constructs a fresh import source using [name] as
    the source's name.  If not given, [name] defaults to [""]. *)

val fresh_from : ?seq:Sym.seq -> ?src:string -> unit -> Syntax.from
(** [fresh_from ?seq ?src ()] generates a fresh from clause using [src] as the
    source name.  If not given, [src] defaults to [""]. *)

val fresh_alias : ?seq:Sym.seq -> ?pkg:string -> ?local:string -> unit -> Syntax.alias
(** [fresh_alias ?seq ?pkg ?local ()] generates a fresh alias clause using [pkg]
    as the package name.  If [local] is not [""], it is used as the local alias.
    (Defaults to [""].) *)

val fresh_pkgs : ?seq:Sym.seq -> ?local:string -> unit -> Syntax.pkgs
(** [fresh_pkgs ?seq ?local ()] generates a fresh list of packages.  The list
    has two elements: the first without a local alias, and the second with.  The
    value of [local] is used as the local alias.  Defaults to [localname]. *)

val fresh_import : ?seq:Sym.seq -> ?from:bool -> ?pkgs:bool -> unit -> Syntax.import
(** [fresh_import ?seq ?from ?pkgs ()] generates a fresh import statement.  If
    [from] is [true], then a from clause is included.  If [pkgs] is [true], then
    a list of packages is included.  Both default to [false]. *)

val fresh_pkg : ?seq:Sym.seq -> ?id:string -> unit -> Syntax.pkg
(** [fresh_pkg ?seq ?id ()] generates a fresh package clause using [id] as the
    package name.  If not provided, [id] defaults to [""]. *)

(** {2 Utilities} *)

(**
 * {3 Location Stripping}
 *
 * Replaces all location information with {!CommonTest.LocTest.dummy}.  Useful
 * for normalizing parsed syntax with hand-constructed syntax in tests.
 *)

val deloc_optional : ('a -> 'a) -> 'a option -> 'a option
(** [deloc_optional deloc value] strips location information from [value] using
    [deloc] if the value is [Some]. *)

val deloc_ty : Syntax.ty -> Syntax.ty
(** [deloc_ty ty] strips location information from the type [ty]. *)

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
