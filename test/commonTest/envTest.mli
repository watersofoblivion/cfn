open Common

open OUnit2

(**
 * {1 Environment}
 *)

val suite : test
(**
  The unit test suite.

  @since 1.0
*)

(**
 * {2 Helpers}
 *)

val fresh : ?seq:Sym.seq -> unit -> 'a Env.t
(**
  Create a fresh environment.

  @param seq The sequence to use when generating identifiers.  Defaults to a
    fresh symbol sequence.
  @return An empty environment
  @since 1.0
*)

(**
 * {2 Assertions}
 *)

val assert_symbol_of : ctxt:test_ctxt -> string -> 'a Env.t -> Sym.t -> unit
(**
  Assert an identifier is bound to a symbol in an environment.

  @param ctxt The testing context
  @param id The identifier expected to be bound
  @param env The environment the identifier is expected to be bound in
  @param expected The symbol the identifier is expected to be bound to
  @raise Failure Raised when the identifier is not bound in the environment or
    is bound to a different symbol
  @since 1.0
*)

val assert_constr_of : ctxt:test_ctxt -> string -> 'a Env.t -> Sym.t -> unit
(**
  Assert an identifier is bound to a type construct in an environment.

  @param ctxt The testing context
  @param id The identifier expected to be bound
  @param env The environment the identifier is expected to be bound in
  @param expected The type constructor the identifier is expected to be bound to
  @raise Failure Raised when the identifier is not bound in the environment or
    is bound to a different type constructor
  @since 1.0
*)

val assert_bound : (ctxt:test_ctxt -> 'a -> 'a -> unit) -> ctxt:test_ctxt -> Sym.t -> 'a Env.t -> 'a -> unit
(**
  Assert a symbol is bound to a value in an environment.

  @param ctxt The testing context
  @param sym The symbol expected to be bound
  @param env The environment the identifier is expected to be bound in
  @param expected The value the identifier is expected to be bound to
  @raise Failure Raised when symbol is not bound in the environment or is bound
    to a different value
  @since 1.0
*)
