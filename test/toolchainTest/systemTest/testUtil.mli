(**
 {1 System Test Helpers}
 *)

open OUnit2

val suite : test
(** [suite] is the suite testing the system test helpers. *)

(**
 {2 Helpers}

 Helper functions for implemeting tests.
 *)

val with_env : string -> string -> ('a -> 'b) -> 'a -> 'b
(** [with_env name value fn x] applies [fn] to [x] with the environment variable
    [name] set to [value] and returns the result.  The environment variable is
    reset to its previous value once [fn] returns, either successfully or with
    an excetion. *)
