(**
 * {1 Abstract Syntax}
 *)

open OUnit2

open Common
open Syntax

(** {2 Test Suite} *)

val suite : test
(** [suite] is the unit test suite. *)

(**
 * {2 Fixtures}
 *
 * Generate fresh, unique values for testing.  All fixtures use
 * {!CommonTest.LocTest.gen} to generate any locations and take a symbol
 * sequence to use for generating fresh names.  If the symbol sequence is not
 * given, a new sequence is created.  The sequence is also passed down to any
 * lower-level fixtures generated as part of a higher-level fixture.
 *)

val fresh_name : ?seq:Sym.seq -> ?id:string -> unit -> Ast.name
(** [fresh_name ?seq ?id ()] constructs a fresh name using [id] as the name.  If
     not provided, [id] defaults to [""]. *)

val fresh_src : ?seq:Sym.seq -> ?name:string -> unit -> Ast.src
(** [fresh_srq ?seq ?name ()] constructs a fresh import source using [name] as
    the source's name.  If not given, [name] defaults to [""]. *)

val fresh_from : ?seq:Sym.seq -> ?src:string -> unit -> Ast.from
(** [fresh_from ?seq ?src ()] generates a fresh from clause using [src] as the
    source name.  If not given, [src] defaults to [""]. *)

val fresh_alias : ?seq:Sym.seq -> ?pkg:string -> ?local:string -> unit -> Ast.alias
(** [fresh_alias ?seq ?pkg ?local ()] generates a fresh alias clause using [pkg]
    as the package name.  If [local] is not [""], it is used as the local alias.
    (Defaults to [""].) *)

val fresh_pkgs : ?seq:Sym.seq -> ?local:string -> unit -> Ast.pkgs
(** [fresh_pkgs ?seq ?local ()] generates a fresh list of packages.  The list
    has two elements: the first without a local alias, and the second with.  The
    value of [local] is used as the local alias.  Defaults to [localname]. *)

val fresh_import : ?seq:Sym.seq -> ?from:bool -> ?pkgs:bool -> unit -> Ast.import
(** [fresh_import ?seq ?from ?pkgs ()] generates a fresh import statement.  If
    [from] is [true], then a from clause is included.  If [pkgs] is [true], then
    a list of packages is included.  Both default to [false]. *)

val fresh_pkg : ?seq:Sym.seq -> ?id:string -> unit -> Ast.pkg
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

val deloc_name : Ast.name -> Ast.name
(** [deloc_name name] strips location information from the name [name]. *)

val deloc_src : Ast.src -> Ast.src
(** [deloc_src src] strips location information from the import source [src]. *)

val deloc_from : Ast.from -> Ast.from
(** [deloc_from from] strips location information from the from clause [from]. *)

val deloc_alias : Ast.alias -> Ast.alias
(** [deloc_alias alias] strips location information from the package alias
    [alias]. *)

val deloc_pkgs : Ast.pkgs -> Ast.pkgs
(** [deloc_pkgs pkgs] strips location information from the package list [pkgs]. *)

val deloc_import : Ast.import -> Ast.import
(** [deloc_import import] strips location information from the import statement
    [import]. *)

val deloc_pkg : Ast.pkg -> Ast.pkg
(** [deloc_pkg pkg] strips location information from the package statement
    [pkg]. *)

val deloc_file : Ast.file -> Ast.file
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

val assert_name_equal : ctxt:test_ctxt -> Ast.name -> Ast.name -> unit
(** [assert_name_equal ~ctxt expected actual] asserts that the name [actual] is
    equal to the name [expected]. *)

val assert_src_equal : ctxt:test_ctxt -> Ast.src -> Ast.src -> unit
(** [assert_src_equal ~ctxt expected actual] asserts that the import source
    [actual] is equal to the import source [expected]. *)

val assert_from_equal : ctxt:test_ctxt -> Ast.from -> Ast.from -> unit
(** [assert_from_equal ~ctxt expected actual] asserts that the from clause
    [actual] is equal to the from clause [expected]. *)

val assert_alias_equal : ctxt:test_ctxt -> Ast.alias -> Ast.alias -> unit
(** [assert_alias_equal ~ctxt expected actual] asserts that the package alias
    [actual] is equal to the package alias [expected]. *)

val assert_pkgs_equal : ctxt:test_ctxt -> Ast.pkgs -> Ast.pkgs -> unit
(** [assert_pkgs_equal ~ctxt expected actual] asserts that the package list
    [actual] is equal to the package list [expected].  This comparison is
    order-dependent. *)

val assert_import_equal : ctxt:test_ctxt -> Ast.import -> Ast.import -> unit
(** [assert_import_equal ~ctxt expected actual] asserts that the import
    statement [actual] is equal to the import statement [expected]. *)

val assert_pkg_equal : ctxt:test_ctxt -> Ast.pkg -> Ast.pkg -> unit
(** [assert_pkg_equal ~ctxt expected actual] asserts that the package statement
    [actual] is equal to the package statement [expected]. *)

val assert_file_equal : ctxt:test_ctxt -> Ast.file -> Ast.file -> unit
(** [assert_file_equal ~ctxt expected actual] asserts that the source file
    [actual] is equal to the source file [expected]. *)
