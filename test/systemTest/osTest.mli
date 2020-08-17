(**
 {1 System Primitives Test Suite}

 Test the low-level operating system functions.
 *)

open OUnit2

val suite : test
(** [suite] is the suite testing the low-level operating system functions. *)

(**
 {2 Assertions}

 Provides assertions useful to other test packages for testing the functions
 privded in the {!Os} package.
 *)

open System

(**
 {3 Process Handling}

 Assertions to test the exit status and output of an executed process.
 *)

val assert_output : (Os.output list -> bytes) -> ctxt:test_ctxt -> msg:string -> Os.output list -> string -> string
(** [assert_output extractor ~ctxt output expected] asserts the the value
    extracted from [output] using [extractor] is equal to [expected].  Returns
    the extracted output. *)

val assert_stdout : ctxt:test_ctxt -> Os.output list -> string -> string
(** [assert_stdout ~ctxt output expected] asserts the standard output in
    [output] is equal to [expected].  Returns the standard output. *)

val assert_stderr : ctxt:test_ctxt -> Os.output list -> string -> string
(** [assert_stderr ~ctxt output expected] asserts the standard error in
    [output] is equal to [expected].  Returns the standard error. *)

val assert_non_zero : ctxt:test_ctxt -> ?stdout:string option -> ?stderr:string option -> int -> (unit -> unit) -> unit
(** [assert_non_zero ~ctxt ?stdout ?stderr status fn] asserts that [fn] raises
    {!Os.NonZero} with an exit status of [status].  If given, asserts that the
    standard output and standard error of the process are [stdout] and [stderr],
    respectively. *)
