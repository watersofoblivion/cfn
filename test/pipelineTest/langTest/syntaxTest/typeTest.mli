(** {1 Types} *)

open OUnit2

open Syntax

(** {2 Test Suite} *)

val suite : test
(** [suite] is the unit test suite. *)

(** {2 Assertions} *)

(** {3 Equality} *)

val assert_ty_equal : ctxt:test_ctxt -> Type.t -> Type.t -> unit
(** [assert_ty_equal ~ctxt expected actual] asserts that the type [actual] is
    equal to the type [expected]. *)
