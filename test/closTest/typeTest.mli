(** {1 Types} *)

open OUnit2

open Clos

(** {2 Test Suite} *)

val suite : test
(** [suite] is the unit test suite. *)

(** {2 Assertions} *)

val assert_ty_equal : ctxt:test_ctxt -> Type.t -> Type.t -> unit
(** [assert_ty_equal ~ctxt expected actual] asserts that the type [actual] is
    equal to the type [expected]. *)
