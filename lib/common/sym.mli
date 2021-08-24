open Format

(**
 * {1 Symbolization}
 *)

(**
 * {2 Symbols}
 *)

type t
(** A symbol *)

(**
 * {3 Operations}
 *)

val sym : t -> int
(** [sym sym] returns the integer identifier of the symbol [sym]. *)

val id : t -> string option
(** [id sym] returns the string identifier associated with the symbol [sym]. *)

(**
 * {2 Sequences}
 *)

type seq
(** A sequence of symbols *)

(**
 * {3 Constructors}
 *)

val seq : unit -> seq
(** [seq ()] generates a new sequence of symbols. *)

(**
 * {3 Operations}
 *)

val gen : ?id:string -> seq -> t
(** [gen ?id seq] generates the next symbol in the sequence [seq].  Guaranteed
    to be distinct from any other symbol generated from [seq].  If an identifier
    [id] is provided, then the identifier is associated with the symbol. *)

(**
 * {2 Operations}
 *)

(**
 * {3 Equality}
 *)

val equal : t -> t -> bool
(** [equal sym sym'] returns [true] if the symbol [sym] is the same symbol as
    [sym'], or [false] otherwise. *)

(**
 * {3 Pretty-Printing}
 *)

val pp : formatter -> t -> unit
(** [pp fmt sym] pretty-prints the symbol [sym] to the formatter [fmt]. If the
    symbol has an associated identifier, the symbol is formatted as
    [<name>$<symbol>] where [symbol] is a number.  Otherwise, the symbol is
    formatted as [$<symbol>]. *)

val pp_id : formatter -> t -> unit
(** [pp_id fmt sym] pretty-prints the identifier associated with symbol [sym] to
    the formatter [fmt].  Raises {!Invalid_argument} if the symbol does not have
    an associated identifier. *)

(**
 * {2 Symbol Tables}
 *)

type 'a tbl
(** A mapping of symbols to values *)

(**
 * {3 Constructors}
 *)

val tbl : 'a tbl
(** [tbl] constructs an empty symbol tbl. *)

(**
 * {3 Operations}
 *)

val bind : t -> 'a -> 'a tbl -> ('a tbl -> 'b) -> 'b
(** [bind sym value tbl kontinue] binds the value [value] to the symbol [sym] in
    the symbol table [tbl].  If the symbol is already bound, a new binding is
    added non-destructively.  The updated table is passed to the continuation
    [kontinue]. *)

val lookup : t -> 'a tbl -> 'a
(** [lookup sym tbl] looks up the value bound to the symbol [sym] in the table
    [tbl].  Raises {!Not_found} if the symbol is not bound in the table. *)
