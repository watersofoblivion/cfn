open OUnit2

open System

(**
 {2 Assertions}
 *)

val assert_output : (Os.output list -> bytes) -> ctxt:test_ctxt -> Os.output list -> string -> unit
val assert_stdout : ctxt:test_ctxt -> Os.output list -> string -> unit
val assert_stderr : ctxt:test_ctxt -> Os.output list -> string -> unit
val assert_non_zero : ctxt:test_ctxt -> ?stdout:string option -> ?stderr:string option -> int -> (unit -> unit) -> unit

(**
 {2 Test Suite}
 *)

val suite : test
