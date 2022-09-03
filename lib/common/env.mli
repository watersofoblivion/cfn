(**
 * {1 Environment}
 *)

type 'a t
(**
  An environment containing bindings.

  @since 1.0
*)

(**
 * {2 Constructors}
 *)

val env : Sym.seq -> 'a t
(**
  Construct an empty environment.

  @param seq The sequence fresh symbols are generated from
  @return An empty environment
  @since 1.0
*)

(**
 * {2 Operations}
 *)

(**
 * {3 Alpha-Renaming}
 *)

val rename : string -> 'a t -> ('a t -> Sym.t -> 'b) -> 'b
(**
  Bind a fresh symbol for an identifier.

  @param id The identifier to bind
  @param env The environment to bind the symbol in
  @param kontinue The continuation to call with the symbol and (possibly
    updated) environment
  @return The result of calling the continuation
  @since 1.0
*)

val symbol_of : string -> 'a t -> Sym.t
(**
  Look up the symbol bound to an identifier.

  @param id The identifier to look up
  @param env The environment to look the symbol up in
  @return The symbol bound to the identifier or a fresh, unbound symbol if the
    identifier is not bound
  @since 1.0
*)

val constr_of : string -> 'a t -> ('a t -> Sym.t -> 'b) -> 'b
(**
  Look up the symbol bound to a type constructor.  If no symbol is bound to the
  type constructor, a fresh symbol is bound.

  @param id The type constructor to look up
  @param env The environment to look the type constructor up in
  @param kontinue The continuation to call with the symbol and (possibly
    updated) environment
  @return The result of calling the continuation
  @since 1.0
*)

(**
 * {3 Type Binding}
 *)

val bind : Sym.t -> 'a -> 'a t -> ('a t -> 'b) -> 'b
(**
  Bind a symbol to a value in an environment.

  @param sym The symbol to bind the value to
  @param value The value to bind to the symbol
  @param env The environment to bind the symbol in
  @param kontinue The continuation to call with the updated environment
  @return The result of calling the continuation
  @since 1.0
*)

val lookup : Sym.t -> 'a t -> 'a
(**
  Look up the value bound to a symbol in an environment.

  @param sym The symbol to look up
  @param env The environment to look the symbol up in
  @return The value the symbol is bound to
  @raise Not_found Raised if the symbol is unbound in the environment
  @since 1.0
*)
