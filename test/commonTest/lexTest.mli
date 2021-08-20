open OUnit2

(**
 * {1 Lexing}
 *)

val suite : test
(** [suite] is the test suite *)

(**
 * {2 Assertions}
 *)

val assert_lexes : ctxt:test_ctxt -> (Lexing.lexbuf -> 'a) -> (ctxt:test_ctxt -> 'a -> 'a -> unit) -> string -> 'a -> unit
(** [assert_lexes ~ctxt lex assert_equal input expected] asserts that the lexer
    [lex] produces the token [expected] when given the input [input].  Token
    equality is asserted with [assert_equal].  The [ctxt] is passed to all
    internal assertions. *)

val assert_lex_lit_pair : ctxt:test_ctxt -> (Lexing.lexbuf -> 'a) -> (ctxt:test_ctxt -> 'a -> 'a -> unit) -> ('b -> 'a) -> ('b * string) list -> unit
(** [assert_lex_lit_pair ~ctxt lex assert_equal construct values] asserts that
    the lexer [lex] produces [construct (fst value)] for each element in
    [values] when given the input [snd value] for corresponding elements in
    [values].  Token equality is asserted with [assert_equal].  The [ctxt] is
    passed to all internal assertions. *)

val assert_lex_lit : ctxt:test_ctxt -> printer:('a -> string) -> (Lexing.lexbuf -> 'b) -> (ctxt:test_ctxt -> 'b -> 'b -> unit) -> ('a -> 'b) -> 'a list -> unit
(** [assert_lex_lit ~ctxt ~printer lex assert_equal construct values] asserts
    that the lexer [lex] produces [construct value] for each element in [values]
    when given the input [printer value] for corresponding elements in [values].
    Token equality is asserted with [assert_equal].  The [ctxt] is passed to all
    internal assertions. *)
