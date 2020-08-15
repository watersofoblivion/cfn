open OUnit2

(**
 {1 Assertions}
 *)

val success : 'a -> unit
(** [success _] returns [()] to end a chain of asserts. *)

(**
 {2 Strings}
 *)

val string_equals : ctxt:test_ctxt -> string -> string -> string
(** [string_equals ~ctxt expected actual] asserts that the string [actual] is
    equal to [expected].  Returns the [actual] value.  This function is tailored
    for simple strings. *)

val text_equals : ctxt:test_ctxt -> string -> string -> string
(** [test_equals ~ctxt expected actual] asserts that the text [actual] is equal
    to [expected].  This function is tailored for multi-line blocks of text and
    displays a diff on error. *)
