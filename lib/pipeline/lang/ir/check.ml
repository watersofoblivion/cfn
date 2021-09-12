(* Type Checking *)

open Common

(* Exceptions *)

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Type.ty * Type.ty
exception InvalidArity of int * int

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise

let invalid_arity expected lst =
  let actual = List.length lst in
  InvalidArity (expected, actual)
    |> raise
