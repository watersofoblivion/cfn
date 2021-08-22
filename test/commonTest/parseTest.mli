open Common

open OUnit2

(**
 * {1 Parsing}
 *)

(**
 * {2 Assertions}
 *)

val assert_parses: ctxt:test_ctxt -> (Lexing.lexbuf -> 'a) -> ((Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'b Env.t -> ('b Env.t -> 'c -> unit) -> unit) -> (ctxt:test_ctxt -> 'c -> 'c -> unit) -> 'b Env.t -> string list -> 'c -> unit
(** [assert_parses ~ctxt lex parse assert_equal env input expected] asserts that
    the parser [parse] produces the [expected] when given the input lines
    [input].  The input is concatenated together with newlines before being
    given to the lexer [lex].  The continuation passed to the parser asserts
    equality on the second argument using [assert_equal]. *)
