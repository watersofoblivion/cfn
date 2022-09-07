(**
  Symbolization
*)

open Format

(**
  {1 Symbols}
*)

type t
(**
  A unique symbol.  A symbol consists of a unique index along with an optional
  string identifier it was derived from.

  @since 1.0
*)

(**
  {2 Operations}
*)

val sym : t -> int
(**
  Get the integer identifier of a symbol.

  @param sym The symbol
  @return The integer identifier of the symbol
  @since 1.0
*)

val id : t -> string option
(**
  Get the string identifier associated with a symbol.

  @param sym The symbol
  @return The string identifier associated with the symbol, or [None] if the
    symbol does not have an associated string identifier.
  @since 1.0
*)

(**
  {1 Sequences}
*)

type seq
(**
  A sequence of unique identifiers used for generating symbols.

  @since 1.0
*)

(**
  {2 Constructors}
*)

val seq : unit -> seq
(**
  Generate a new sequence of symbols.

  @return A fresh sequence of symbols
  @since 1.0
*)

(**
  {2 Operations}
*)

val gen : ?id:string -> seq -> t
(**
  Generate the next symbol in a sequence.  Guaranteed to be distinct from any
  other symbol generated from the same sequence.

  @param id The optional string identifier associated with the symbol.
  @param seq The sequence to generate the symbol from
  @return A fresh symbol
  @since 1.0
*)

(**
  {1 Operations}
*)

(**
  {2 Equality}
*)

val equal : t -> t -> bool
(**
  Test if two symbols are equivalent.

  @param sym The first symbol
  @param sym' The second symbol
  @return [true] if the symbols are the equivalent, [false] otherwise
  @since 1.0
*)

(**
  {2 Pretty-Printing}
*)

val pp : formatter -> t -> unit
(**
  Pretty-print a symbol to a formatter. If the symbol has an associated
  identifier, the symbol is formatted as [<name>$<idx>] where [idx] is a
  distinct value from the sequence used to generate this symbol.  Otherwise, the
  symbol is formatted as [$<idx>].

  @param fmt The formatter to print to
  @param sym The symbol to print
  @since 1.0
*)

val pp_id : formatter -> t -> unit
(**
  Pretty-print the identifier associated with a symbol to a formatter.

  @param fmt The formatter to print to
  @param sym The symbol to print
  @raise Invalid_argument Raised if the symbol does not have an associated
    identifier
  @see "pp"
  @since 1.0
*)

(**
  {1 Symbol Tables}
*)

type 'a tbl
(**
  A mapping of symbols to values.

  @since 1.0
*)

(**
  {2 Constructors}
*)

val tbl : 'a tbl
(**
  An empty symbol table.

  @since 1.0
*)

(**
  {2 Operations}
*)

val bind : t -> 'a -> 'a tbl -> ('a tbl -> 'b) -> 'b
(**
  Bind a value to a symbol in a symbol table.  If the symbol is already bound, a
  new binding is added non-destructively.

  @param sym The symbol to bind the value to
  @param value The value to bind to the symbol
  @param tbl The table to bind the value in
  @param kontinue The continuation to pass the updated table to
  @return The result of calling the continuation
  @since 1.0
*)

val lookup : t -> 'a tbl -> 'a
(**
  Look up the value bound to a symbol in a table.

  @param sym The symbol to look up
  @param tbl The table to look the symbol up in
  @return The value bound to the symbol
  @raise Not_found Raised if the symbol is not bound in the table
  @since 1.0
*)
