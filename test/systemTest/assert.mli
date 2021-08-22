open OUnit2

(**
 {1 Assertions}
 *)

(**
 {2 Generic}
 *)

val should : (ctxt:test_ctxt -> 'a -> 'a -> 'a) -> ('b -> 'a) -> ctxt:test_ctxt -> 'a -> 'b -> 'b
(** [should matcher extractor ~ctxt expected value] asserts that the result of
    applying [extractor] to [value] matches [expected] using the supplied
    [matcher].  Returns [value] on success. *)

val success : 'a -> unit
(** [success _] returns [()] to end a chain of asserts. *)

(**
 {2 Strings}
 *)

val string_equals : ctxt:test_ctxt -> ?msg:string -> string -> string -> string
(** [string_equals ~ctxt ?msg expected actual] asserts that the string [actual]
    is equal to [expected].  If provided, use [msg] as the error message.
    Returns the [actual] value.  This function is tailored for simple strings. *)

val text_equals : ctxt:test_ctxt -> ?msg:string -> string -> string -> string
(** [test_equals ~ctxt ?msg expected actual] asserts that the text [actual] is
    equal to [expected].  If provided, use [msg] as the error message.  This
    function is tailored for multi-line blocks of text and displays a diff when
    the two strings are different. *)

(**
 {2 Exceptions}
 *)

val fails_with : string -> (unit -> unit) -> unit
(** [fails_with msg fn] asserts that the function [fn] raises a {!Failure}
    exception with the message [msg]. *)

val invalid_argument : string -> (unit -> unit) -> unit
(** [invalid_argument msg fn] asserts that the function [fn] raises an
    {!Invalid_argument} exception with the message [msg]. *)
