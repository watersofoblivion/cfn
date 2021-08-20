open Common

open OUnit2

(**
 * {1 Environment}
 *)

val suite : test
(** [suite] is the test suite *)

(**
 * {2 Helpers}
 *)

val fresh : unit -> 'a Env.t
(** [fresh _] returns an empty environment using a fresh symbol sequence. *)

(**
 * {2 Assertions}
 *)

val assert_symbol_of : ctxt:test_ctxt -> string -> 'a Env.t -> Sym.t -> unit
(** [assert_symbol_of ~ctxt id env expected] asserts that the identifier [id]
    is bound to the symbol [expected] in the environment [env].  The [ctxt] is
    passed to all internal assertions. *)

val assert_constr_of : ctxt:test_ctxt -> string -> 'a Env.t -> Sym.t -> unit
(** [assert_constr_of ~ctxt id env expected] asserts that the identifier [id]
    is bound to the constructor symbol [expected] in the environment [env].  The
    [ctxt] is passed to all internal assertions. *)

val assert_bound : (ctxt:test_ctxt -> 'a -> 'a -> unit) -> ctxt:test_ctxt -> Sym.t -> 'a Env.t -> 'a -> unit
(** [assert_bound assert_equal ~ctxt sym env expected] asserts that the symbol
    [sym] is bound to the value [expected] in the environment [env].  The
    equality of the expected and actual values is asserted with [assert_equal].
    The [ctxt] is passed to all internal assertions. *)
