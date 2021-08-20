(**
 * {1 Environment}
 *)

type 'a t
(** An environment *)

(**
 * {2 Constructors}
 *)

val env : Sym.seq -> 'a t
(** [env seq] constructs an empty environment which generates symbols from the
    sequence [seq]. *)

(**
 * {2 Operations}
 *)

(**
 * {3 Alpha-Renaming}
 *)

val rename : string -> 'a t -> ('a t -> Sym.t -> 'b) -> 'b
(** [rename id env kontinue] binds a fresh symbol for the identifier [id] and
    binds it in the environment [env].  The symbol and the updated environment
    are passed to the continuation [kontinue]. *)

val symbol_of : string -> 'a t -> Sym.t
(** [symbol_of id env] looks up the symbol for the identifier [id] in the
    environment [env].  If no symbol is bound to that identifier, a fresh
    unbound symbol is returned. *)

val constr_of : string -> 'a t -> ('a t -> Sym.t -> 'b) -> 'b
(** [constr_of id env kontinue] looks up the symbol for the type constructor
    [id] in the environment [env].  If no symbol is bound to the constructor, a
    fresh symbol is bound.  The symbol and the (possibly updated) environment
    are passed to the continuation [kontinue]. *)

(**
 * {3 Type Binding}
 *)

val bind : Sym.t -> 'a -> 'a t -> ('a t -> 'b) -> 'b
(** [bind sym value env kontinue] binds the symbol [sym] to the value [value] in
    the environment [env].  The updated environment is passed to the
    continuation [kontinue]. *)

val lookup : Sym.t -> 'a t -> 'a
(** [lookup sym env] looks up the value bound to the symbol [sym] in the
    environment [env].  Raises {!Not_found} is the symbol is not bound. *)
