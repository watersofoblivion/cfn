(**
  Abstract Syntax Tests
*)

open OUnit2

open Common

(**
  {1 Unit Test Suite}
*)

val suite : test
(**
  The unit test suite.

  @since 1.0
*)

(**
  {1 Fixtures}

  Generate fresh, unique values for testing.  All fixtures use
  {!CommonTest.LocTest.gen} to generate any locations and take a symbol
  sequence to use for generating fresh names.  If the symbol sequence is not
  given, the default sequence is used.  The sequence is also passed down to any
  lower-level fixtures generated as part of a higher-level fixture.
*)

(**
  {2 Types}
*)

val fresh_ty_constr : ?loc:Loc.t -> ?id:Sym.t -> unit -> Syntax.ty
(**
  Construct a fresh type constructor.

  @param loc The location of the type constructor
  @param id The name of the type constructor
  @return A fresh type constructor
  @since 1.0
*)

(**
  {2 Operators}
*)

(**
  {2 Unary}
*)

val fresh_un_neg : ?loc:Loc.t -> unit -> Syntax.un
(**
  Construct a fresh unary negation operator.

  @param loc The location of the operator
  @return A fresh unary negation operator
  @since 1.0
*)

val fresh_un_log_not : ?loc:Loc.t -> unit -> Syntax.un
(**
  Construct a fresh logical NOT operator.

  @param loc The location of the operator
  @return A fresh logical NOT operator
  @since 1.0
*)

val fresh_un_bit_not : ?loc:Loc.t -> unit -> Syntax.un
(**
  Construct a fresh bitwise NOT operator.

  @param loc The location of the operator
  @return A fresh bitwise NOT operator
  @since 1.0
*)

(**
  {2 Binary}
*)

val fresh_bin_struct_eq : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh structural equality operator.

  @param loc The location of the operator
  @return A fresh structural equality operator
  @since 1.0
*)

val fresh_bin_struct_neq : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh structural inequality operator.

  @param loc The location of the operator
  @return A fresh structural inequality operator
  @since 1.0
*)

val fresh_bin_phys_eq : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh physical equality operator.

  @param loc The location of the operator
  @return A fresh physical equality operator
  @since 1.0
*)

val fresh_bin_phys_neq : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh physical inequality operator.

  @param loc The location of the operator
  @return A fresh physical inequality operator
  @since 1.0
*)

val fresh_bin_lt : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh less than operator.

  @param loc The location of the operator
  @return A fresh less than operator
  @since 1.0
*)

val fresh_bin_lte : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh less than or equal operator.

  @param loc The location of the operator
  @return A fresh less than or equal operator
  @since 1.0
*)

val fresh_bin_gt : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh greater than operator.

  @param loc The location of the operator
  @return A fresh greater than operator
  @since 1.0
*)

val fresh_bin_gte : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh greater than or equal operator.

  @param loc The location of the operator
  @return A fresh greater than or equal operator
  @since 1.0
*)

val fresh_bin_lsl : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh logical shift left operator.

  @param loc The location of the operator
  @return A fresh logical shift left operator
  @since 1.0
*)

val fresh_bin_lsr : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh logical shift right operator.

  @param loc The location of the operator
  @return A fresh logical shift right operator
  @since 1.0
*)

val fresh_bin_asl : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh arithmetic shift left operator.

  @param loc The location of the operator
  @return A fresh arithmetic shift left operator
  @since 1.0
*)

val fresh_bin_asr : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh arithmetic shift right operator.

  @param loc The location of the operator
  @return A fresh arithmetic shift right operator
  @since 1.0
*)

val fresh_bin_add : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh addition operator.

  @param loc The location of the operator
  @return A fresh addition operator
  @since 1.0
*)

val fresh_bin_sub : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh subtraction operator.

  @param loc The location of the operator
  @return A fresh subtraction operator
  @since 1.0
*)

val fresh_bin_mul : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh multiplication operator.

  @param loc The location of the operator
  @return A fresh multiplication operator
  @since 1.0
*)

val fresh_bin_div : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh division operator.

  @param loc The location of the operator
  @return A fresh division operator
  @since 1.0
*)

val fresh_bin_mod : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh modulus operator.

  @param loc The location of the operator
  @return A fresh modulus operator
  @since 1.0
*)

val fresh_bin_exp : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh exponentiation operator.

  @param loc The location of the operator
  @return A fresh exponentiation operator
  @since 1.0
*)

val fresh_bin_log_and : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh logical AND operator.

  @param loc The location of the operator
  @return A fresh logical AND operator
  @since 1.0
*)

val fresh_bin_log_or : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh logical OR operator.

  @param loc The location of the operator
  @return A fresh logical OR operator
  @since 1.0
*)

val fresh_bin_bit_and : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh bitwise AND operator.

  @param loc The location of the operator
  @return A fresh bitwise AND operator
  @since 1.0
*)

val fresh_bin_bit_or : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh bitwise OR operator.

  @param loc The location of the operator
  @return A fresh bitwise OR operator
  @since 1.0
*)

val fresh_bin_bit_xor : ?loc:Loc.t -> unit -> Syntax.bin
(**
  Construct a fresh bitwise XOR operator.

  @param loc The location of the operator
  @return A fresh bitwise XOR operator
  @since 1.0
*)

(**
  {2 Patterns}
*)

val fresh_patt_ground : ?loc:Loc.t -> unit -> Syntax.patt
(**
  Construct a fresh ground ([_]) pattern.

  @param loc The location of the pattern
  @return A fresh ground pattern
  @since 1.0
*)

val fresh_patt_var : ?loc:Loc.t -> ?id:Sym.t -> unit -> Syntax.patt
(**
  Construct a fresh identifier pattern.

  @param loc The location of the pattern
  @param id The identifier to bind
  @return A fresh identifier pattern
  @since 1.0
*)

(**
  {2 Runes}
*)

val fresh_rune_lit : ?loc:Loc.t -> ?value:char -> unit -> Syntax.rune
(**
  Construct a fresh rune literal.

  @param loc The location of the literal
  @param value The value of the literal
  @return A fresh rune literal
  @since 1.0
*)

val fresh_rune_escape : ?loc:Loc.t -> ?value:int -> unit -> Syntax.rune
(**
  Construct a fresh Unicode escape sequence rune literal.

  @param loc The location of the literal
  @param value The escape sequence
  @return A fresh Unicode escape sequence rune literal
  @since 1.0
*)

(**
  {2 Strings}
*)

val fresh_str_lit : ?loc:Loc.t -> ?value:string -> unit -> Syntax.str
(**
  Construct a fresh string literal string segment.

  @param loc The location of the segment
  @param value The value of the literal segment
  @return A fresh string literal string segment
  @since 1.0
*)

val fresh_str_escape : ?loc:Loc.t -> ?value:int -> unit -> Syntax.str
(**
  Construct a fresh Unicode escape sequence string segment.

  @param loc The location of the segment
  @param value The escape sequence
  @return A fresh Unicode escape sequence string segment
  @since 1.0
*)

(**
  {2 Expressions}
*)

val fresh_expr_bool : ?loc:Loc.t -> ?value:bool -> unit -> Syntax.expr
(**
  Construct a fresh boolean literal.

  @param loc The location of the literal
  @param value The value of the literal
  @return A fresh boolean literal
  @since 1.0
*)

val fresh_expr_int : ?loc:Loc.t -> ?value:int32 -> unit -> Syntax.expr
(**
  Construct a fresh 32-bit integer literal.

  @param loc The location of the literal
  @param value The value of the literal
  @return A fresh integer literal
  @since 1.0
*)

val fresh_expr_long : ?loc:Loc.t -> ?value:int64 -> unit -> Syntax.expr
(**
  Construct a fresh 64-bit integer literal.

  @param loc The location of the literal
  @param value The value of the literal
  @return A fresh long literal
  @since 1.0
*)

val fresh_expr_float : ?loc:Loc.t -> ?value:float -> unit -> Syntax.expr
(**
  Construct a fresh single-precision floating-point literal.

  @param loc The location of the literal
  @param value The value of the literal
  @return A fresh float literal
  @since 1.0
*)

val fresh_expr_double : ?loc:Loc.t -> ?value:float -> unit -> Syntax.expr
(**
  Construct a fresh double-precision floating-point literal.

  @param loc The location of the literal
  @param value The value of the literal
  @return A fresh double literal
  @since 1.0
*)

val fresh_expr_rune : ?loc:Loc.t -> ?value:Syntax.rune -> unit -> Syntax.expr
(**
  Construct a fresh rune literal.

  @param loc The location of the literal
  @param value The value of the literal
  @return A fresh rune literal
  @since 1.0
*)

val fresh_expr_string : ?loc:Loc.t -> ?value:(Syntax.str list list) -> unit -> Syntax.expr
(**
  Construct a fresh string literal.

  @param loc The location of the literal
  @param value A list of lines, each given as a list of string segments
  @return A fresh string literal
  @since 1.0
*)

val fresh_expr_ident : ?loc:Loc.t -> ?seq:Sym.seq -> ?id:string -> unit -> Syntax.expr
(**
  Construct a fresh identifier.

  @param loc The location of the literal
  @param seq The sequence to draw a symbol index from
  @param id The name of the identifier
  @return A fresh identifier
  @since 1.0
*)

val fresh_expr_un_op : ?loc:Loc.t -> ?op:Syntax.un -> ?operand:Syntax.expr -> unit -> Syntax.expr
(**
  Construct a fresh unary operator expression.

  @param loc The location of the literal
  @param op The operator
  @param operand The operand
  @return A fresh unary operator expression
  @since 1.0
*)

val fresh_expr_bin_op : ?loc:Loc.t -> ?op:Syntax.bin -> ?lhs:Syntax.expr -> ?rhs:Syntax.expr -> unit -> Syntax.expr
(**
  Construct a fresh binary operator expression.

  @param loc The location of the literal
  @param op The operator
  @param lhs The left operand
  @param rhs The right operand
  @return A fresh binary operator expression
  @since 1.0
*)

val fresh_expr_let : ?loc:Loc.t -> ?binding:Syntax.binding -> ?scope:Syntax.expr -> unit -> Syntax.expr
(**
  Construct a fresh let binding expression.

  @param loc The location of the let binding
  @param binding The value binding
  @param scope The scope of the binding
  @return A fresh let binding expression
  @since 1.0
*)

(**
  {2 Bindings}
*)

val fresh_value_binding : ?loc:Loc.t -> ?patt:Syntax.patt -> ?explicit:bool -> ?ty:Syntax.ty -> ?value:Syntax.expr -> unit -> Syntax.binding
(**
  Construct a fresh value binding.

  @param loc The location of the let binding
  @param patt The pattern being bound to
  @param explicit Include a type annotation
  @param ty The annotated type
  @param value The value being bound
  @return A fresh value binding
  @since 1.0
*)

(**
  {2 Top-Level Expressions}
*)

val fresh_top_let : ?loc:Loc.t -> ?binding:Syntax.binding -> unit -> Syntax.top
(**
  Construct a fresh top-level let binding.

  @param loc The location of the top-level binding
  @param binding The value binding
  @return A fresh top-level let binding
  @since 1.0
*)

val fresh_top_val : ?loc:Loc.t -> ?binding:Syntax.binding -> unit -> Syntax.top
(**
  Construct a fresh top-level value binding.

  @param loc The location of the top-level binding
  @param binding The value binding
  @return A fresh top-level value binding
  @since 1.0
*)

(**
  {2 Imports}
*)

val fresh_name : ?loc:Loc.t -> ?seq:Sym.seq -> ?id:string -> unit -> Syntax.name
(**
  Construct a fresh name.

  @param loc The location of the name
  @param seq The sequence to draw the symbol index from
  @param id The name of the symbol
  @return A fresh name
  @since 1.0
*)

val fresh_proto : ?loc:Loc.t -> ?proto:string -> unit -> Syntax.proto
(**
  Construct a fresh protocol specifier.

  @param loc The location of the protocol
  @param seq The protocol specifier
  @return A fresh protocol specifier
  @since 1.0
*)

val fresh_host : ?loc:Loc.t -> ?host:string -> unit -> Syntax.host
(**
  Construct a fresh host identifier.

  @param loc The location of the identifier
  @param seq The host
  @return A fresh host identifier
  @since 1.0
*)

val fresh_hostpath : ?loc:Loc.t -> ?segs:(string list) -> unit -> Syntax.hostpath
(**
  Construct a fresh host path.

  @param loc The location of the path
  @param segs The path segments
  @return A fresh host path
  @since 1.0
*)

val fresh_version : ?loc:Loc.t -> ?version:string -> unit -> Syntax.version
(**
  Construct a fresh major version.

  @param loc The location of the version
  @param version The version
  @return A fresh major version
  @since 1.0
*)

val fresh_src_current : ?loc:Loc.t -> unit -> Syntax.src
(**
  Construct a fresh current source reference.

  @param loc The location of the source
  @return A fresh current source reference
  @since 1.0
*)

val fresh_src_external : ?loc:Loc.t -> ?with_proto:bool -> ?proto:Syntax.proto -> ?host:Syntax.host -> ?with_path:bool -> ?path:Syntax.hostpath -> ?version:Syntax.version -> unit -> Syntax.src
(**
  Construct a fresh external source reference.

  @param loc The location of the source
  @param with_proto Include the protocol
  @param proto The protocol specifier
  @param host The host identifier
  @param with_path Include a host path
  @param path The host path
  @param version The major version
  @return A fresh external source reference
  @since 1.0
*)

val fresh_from : ?loc:Loc.t -> ?src:Syntax.src -> unit -> Syntax.from
(**
  Construct a fresh [from] clause.

  @param loc The location of the from clause
  @param src The source reference
  @return A fresh from clause
  @since 1.0
*)

val fresh_pkgpath : ?loc:Loc.t -> ?path:(Syntax.str list) -> unit -> Syntax.pkgpath
(**
  Construct a fresh package path.

  @param loc The location of the path
  @param path The path segments
  @return A fresh package path
  @since 1.0
*)

val fresh_alias : ?loc:Loc.t -> ?pkg:Syntax.pkgpath -> ?with_alias:bool -> ?alias:Syntax.name -> unit -> Syntax.alias
(**
  Construct a fresh package alias clause.

  @param loc The location of the alias clause
  @param pkg The package path
  @param with_alias Include a local alias
  @param alias The local alias
  @return A fresh package alias clause
  @since 1.0
*)

val fresh_pkgs : ?loc:Loc.t -> ?aliases:(Syntax.alias list) -> unit -> Syntax.pkgs
(**
  Construct a fresh package list.

  @param loc The location of the list
  @param aliases The list of package alias clauses
  @return A fresh package list
  @since 1.0
*)

val fresh_import : ?loc:Loc.t -> ?stdlib:bool -> ?from:Syntax.from -> ?pkgs:Syntax.pkgs -> unit -> Syntax.import
(**
  Construct a fresh [import] statement.

  @param loc The location of the import statement
  @param stdlib This import is from the standard library so ignore the [from]
    clause
  @param from The from clause
  @param pkgs The package list
  @return A fresh import statement
  @since 1.0
*)

val fresh_pkg : ?loc:Loc.t -> ?id:Syntax.name -> unit -> Syntax.pkg
(**
  Construct a fresh [package] statement.

  @param loc The location of the package statement
  @param id The name of the package
  @return A fresh package statement
  @since 1.0
*)

(**
  {1 Utilities}
*)

(**
  {2 Location Stripping}

  Replaces all location information with {!CommonTest.LocTest.dummy}.  Useful
  for normalizing parsed syntax with hand-constructed syntax in tests.
 *)

val deloc_ty : Syntax.ty -> Syntax.ty
(**
  Strip location information from a type.

  @param ty The type with location information
  @return The type stripped of location information
  @since 1.0
*)

val deloc_un : Syntax.un -> Syntax.un
(**
  Strip location information from a unary operator.

  @param op The unary operator with location information
  @return The unary operator stripped of location information
  @since 1.0
*)

val deloc_bin : Syntax.bin -> Syntax.bin
(**
  Strip location information from a binary operator.

  @param op The binary operator with location information
  @return The binary operator stripped of location information
  @since 1.0
*)

val deloc_expr : Syntax.expr -> Syntax.expr
(**
  Strip location information from an expression.

  @param expr The expression with location information
  @return The expression stripped of location information
  @since 1.0
*)

val deloc_patt : Syntax.patt -> Syntax.patt
(**
  Strip location information from a pattern.

  @param patt The pattern with location information
  @return The pattern stripped of location information
  @since 1.0
*)

val deloc_binding : Syntax.binding -> Syntax.binding
(**
  Strip location information from a value binding.

  @param binding The value binding with location information
  @return The value binding stripped of location information
  @since 1.0
*)

val deloc_top : Syntax.top -> Syntax.top
(**
  Strip location information from a top-level expression.

  @param top The top-level expression with location information
  @return The top-level expression stripped of location information
  @since 1.0
*)

val deloc_name : Syntax.name -> Syntax.name
(**
  Strip location information from a name.

  @param name The name with location information
  @return The name stripped of location information
  @since 1.0
*)

val deloc_proto : Syntax.proto -> Syntax.proto
(**
  Strip location information from a protocol specifier.

  @param proto The protocol specifier with location information
  @return The protocol specifier stripped of location information
  @since 1.0
*)

val deloc_host : Syntax.host -> Syntax.host
(**
  Strip location information from a host identifier.

  @param host The host identifier with location information
  @return The host identifier stripped of location information
  @since 1.0
*)

val deloc_hostpath : Syntax.hostpath -> Syntax.hostpath
(**
  Strip location information from a host path.

  @param hostpath The host path with location information
  @return The host path stripped of location information
  @since 1.0
*)

val deloc_version : Syntax.version -> Syntax.version
(**
  Strip location information from a major version.

  @param version The major version with location information
  @return The major version stripped of location information
  @since 1.0
*)

val deloc_src : Syntax.src -> Syntax.src
(**
  Strip location information from a source reference.

  @param src The source reference with location information
  @return The source reference stripped of location information
  @since 1.0
*)

val deloc_from : Syntax.from -> Syntax.from
(**
  Strip location information from a [from] clause.

  @param from The from clause with location information
  @return The from clause stripped of location information
  @since 1.0
*)

val deloc_pkgpath : Syntax.pkgpath -> Syntax.pkgpath
(**
  Strip location information from a package path.

  @param path The package path with location information
  @return The package path stripped of location information
  @since 1.0
*)

val deloc_alias : Syntax.alias -> Syntax.alias
(**
  Strip location information from a package alias clause.

  @param alias The package alias clause with location information
  @return The package alias clause stripped of location information
  @since 1.0
*)

val deloc_pkgs : Syntax.pkgs -> Syntax.pkgs
(**
  Strip location information from a package alias list.

  @param pkgs The package alias list with location information
  @return The package alias list stripped of location information
  @since 1.0
*)

val deloc_import : Syntax.import -> Syntax.import
(**
  Strip location information from an import statement.

  @param import The import statement with location information
  @return The import statement stripped of location information
  @since 1.0
*)

val deloc_pkg : Syntax.pkg -> Syntax.pkg
(**
  Strip location information from a package statement.

  @param pkg The package statement with location information
  @return The package statement stripped of location information
  @since 1.0
*)

val deloc_file : Syntax.file -> Syntax.file
(**
  Strip location information from a source file.

  @param file The source file with location information
  @return The source file stripped of location information
  @since 1.0
*)

(**
  {1 Assertions}
*)

(**
  {2 Equality}

  All equality assertions include location equality.  To normalize location
  information, use the [deloc_...] family of functions.

  The test context passed in is passed down to all internal assertions.
 *)

val assert_ty_equal : ctxt:test_ctxt -> Syntax.ty -> Syntax.ty -> unit
(**
  Assert that two type constructors are equal.

  The two values are considered equal if they agree on location and name.

  @param ctxt The testing context
  @param expected The expected type constructor
  @param actual The actual type constructor
  @raise Failure If the type constructors disagree on location or name
  @since 1.0
*)

val assert_un_equal : ctxt:test_ctxt -> Syntax.un -> Syntax.un -> unit
(**
  Assert that two unary operators are equal.

  The two values are considered equal if they are the same operator at the same
  location.

  @param ctxt The testing context
  @param expected The expected operator
  @param actual The actual operator
  @raise Failure If the operators disagree on location or name
  @since 1.0
*)

val assert_bin_equal : ctxt:test_ctxt -> Syntax.bin -> Syntax.bin -> unit
(**
  Assert that two binary operators are equal.

  The two values are considered equal if they are the same operator at the same
  location.

  @param ctxt The testing context
  @param expected The expected operator
  @param actual The actual operator
  @raise Failure If the operators disagree on location or name
  @since 1.0
*)

val assert_rune_equal : ctxt:test_ctxt -> Syntax.rune -> Syntax.rune -> unit
(**
  Assert that two runes are equal.

  The two values are considered equal if they agree on location, kind (Unicode
  character vs. escape sequence,) and value.

  @param ctxt The testing context
  @param expected The expected rune
  @param actual The actual rune
  @raise Failure If the runes disagree on location, kind, or value
  @since 1.0
*)

val assert_str_equal : ctxt:test_ctxt -> Syntax.str -> Syntax.str -> unit
(**
  Assert that two string segments are equal.

  The two values are considered equal if they agree on location, kind (literal
  vs. escape sequence,) and value.

  @param ctxt The testing context
  @param expected The expected segment
  @param actual The actual segment
  @raise Failure If the segments disagree on location, kind, or value
  @since 1.0
*)

val assert_expr_equal : ctxt:test_ctxt -> Syntax.expr -> Syntax.expr -> unit
(**
  Assert that two expressions are equal.

  The two values are considered equal if they agree on location, kind (binary
  operation, let binding, etc.,) and all subcomponents, for example operands to a
  binary operator must match pairwise.

  @param ctxt The testing context
  @param expected The expected expression
  @param actual The actual expression
  @raise Failure If the expressions disagree on location, kind, or any
    subcomponent
  @since 1.0
*)

val assert_patt_equal : ctxt:test_ctxt -> Syntax.patt -> Syntax.patt -> unit
(**
  Assert that two patterns are equal.

  The two values are considered equal if they agree on location, kind (ground,
  identifier, etc.,) and all subcomponents, for example the symbols of identifier
  patterns must match.

  @param ctxt The testing context
  @param expected The expected pattern
  @param actual The actual pattern
  @raise Failure If the patterns disagree on location, kind, or any subcomponent
  @since 1.0
*)

val assert_binding_equal : ctxt:test_ctxt -> Syntax.binding -> Syntax.binding -> unit
(**
  Assert that two value bindings are equal.

  The two values are considered equal if they agree on the location, pattern to
  bind to, whether there is a type annotation and the annotated type, and the
  value to be bound.

  @param ctxt The testing context
  @param expected The expected binding
  @param actual The actual binding
  @raise Failure If the bindings disagree on location, pattern, type, or bound
    value
  @since 1.0
*)

val assert_top_equal : ctxt:test_ctxt -> Syntax.top -> Syntax.top -> unit
(**
  Assert that two top-level bindings are equal.

  The two values are considered equal if they agree on location, kind (let, val,
  etc.,) and on the binding.

  @param ctxt The testing context
  @param expected The expected binding
  @param actual The actual binding
  @raise Failure If the bindings disagree on location, kind, or binding
  @since 1.0
*)

val assert_name_equal : ctxt:test_ctxt -> Syntax.name -> Syntax.name -> unit
(**
  Assert that two names are equal.

  The two values are considered equal if they agree on location and the symbol.

  @param ctxt The testing context
  @param expected The expected name
  @param actual The actual name
  @raise Failure If the names disagree on location or symbol
  @since 1.0
*)

val assert_proto_equal : ctxt:test_ctxt -> Syntax.proto -> Syntax.proto -> unit
(**
  Assert that two protocol specifiers are equal.

  The two values are considered equal if they agree on location and protocol
  name.

  @param ctxt The testing context
  @param expected The expected protocol
  @param actual The actual protocol
  @raise Failure If the protocol specifiers disagree on location or protocol
    name
  @since 1.0
*)

val assert_host_equal : ctxt:test_ctxt -> Syntax.host -> Syntax.host -> unit
(**
  Assert that two host identifiers are equal.

  The two values are considered equal if they agree on location and host name.

  @param ctxt The testing context
  @param expected The expected host
  @param actual The actual host
  @raise Failure If the host identifiers disagree on location or host name
  @since 1.0
*)

val assert_hostpath_equal : ctxt:test_ctxt -> Syntax.hostpath -> Syntax.hostpath -> unit
(**
  Assert that two host paths are equal.

  The two values are considered equal if they agree on location and all segments
  are pairwise equal.

  @param ctxt The testing context
  @param expected The expected path
  @param actual The actual path
  @raise Failure If the host paths disagree on location or any segment
  @since 1.0
*)

val assert_version_equal : ctxt:test_ctxt -> Syntax.version -> Syntax.version -> unit
(**
  Assert that two major versions are equal.

  The two values are considered equal if they agree on location and version
  number.

  @param ctxt The testing context
  @param expected The expected version
  @param actual The actual version
  @raise Failure If the major versions disagree on location or version number
  @since 1.0
*)

val assert_src_equal : ctxt:test_ctxt -> Syntax.src -> Syntax.src -> unit
(**
  Assert that two source references are equal.

  The two values are considered equal if they agree on location, kind (current
  vs. external,) and, for external references, on protocol specifier, host
  identifier, host path, and major version.

  @param ctxt The testing context
  @param expected The expected source
  @param actual The actual source
  @raise Failure If the source references disagree on location, kind, protocol
    specifier, host identifier, host path, or major version
  @since 1.0
*)

val assert_from_equal : ctxt:test_ctxt -> Syntax.from -> Syntax.from -> unit
(**
  Assert that two [from] clauses are equal.

  The two values are considered equal if they agree on location and source
  reference.

  @param ctxt The testing context
  @param expected The expected from clause
  @param actual The actual from clause
  @raise Failure If the from clauses disagree on location or source reference
  @since 1.0
*)

val assert_pkgpath_equal : ctxt:test_ctxt -> Syntax.pkgpath -> Syntax.pkgpath -> unit
(**
  Assert that two package paths are equal.

  The two values are considered equal if they agree on location and all path
  segments.

  @param ctxt The testing context
  @param expected The expected path
  @param actual The actual path
  @raise Failure If the package paths disagree on location or any path segment
  @since 1.0
*)

val assert_alias_equal : ctxt:test_ctxt -> Syntax.alias -> Syntax.alias -> unit
(**
  Assert that two package alias clauses are equal.

  The two values are considered equal if they agree on location, package path,
  and the presence and value of a local alias.

  @param ctxt The testing context
  @param expected The expected alias clause
  @param actual The actual alias clause
  @raise Failure If the alias clauses disagree on location, package path, or
    presence or value of a local alias
  @since 1.0
*)

val assert_pkgs_equal : ctxt:test_ctxt -> Syntax.pkgs -> Syntax.pkgs -> unit
(**
  Assert that two package alias clause lists are equal.

  The two values are considered equal if they agree on location and all package
  alias clauses, in order.

  @param ctxt The testing context
  @param expected The expected alias clause list
  @param actual The actual alias clause list
  @raise Failure If the alias clause lists disagree on location, or any package
    alias clauses or their order
  @since 1.0
*)

val assert_import_equal : ctxt:test_ctxt -> Syntax.import -> Syntax.import -> unit
(**
  Assert that two [import] statements are equal.

  The two values are considered equal if they agree on location, from clause,
  and package alias clause lists.

  @param ctxt The testing context
  @param expected The expected import statement
  @param actual The actual import statement
  @raise Failure If the import statements disagree on location, from clause, or
    their package alias clause lists
  @since 1.0
*)

val assert_pkg_equal : ctxt:test_ctxt -> Syntax.pkg -> Syntax.pkg -> unit
(**
  Assert that two [package] statements are equal.

  The two values are considered equal if they agree on location and name.

  @param ctxt The testing context
  @param expected The expected package statement
  @param actual The actual package statement
  @raise Failure If the package statements disagree on location or name
  @since 1.0
*)

val assert_file_equal : ctxt:test_ctxt -> Syntax.file -> Syntax.file -> unit
(**
  Assert that two source files are equal.

  The two values are considered equal if they agree on location, package
  statement, all import statements and their order, and all top-level bindings
  and their order.

  @param ctxt The testing context
  @param expected The expected file
  @param actual The actual file
  @raise Failure If the source files disagree on package statement, import
    statements or their order, or top-level bindings or their order
  @since 1.0
*)
