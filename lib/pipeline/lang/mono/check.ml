(* Type Checking *)

open Common

(* Exceptions *)

exception UnboundIdentifier of { id: Sym.t }
exception MismatchedTypes of { inferred: Type.ty; annotated: Type.ty }
exception InvalidArity of { expected: int; actual: int }

let unbound_identifier id =
  UnboundIdentifier { id }
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes { inferred; annotated }
    |> raise

let invalid_arity expected lst =
  let actual = List.length lst in
  InvalidArity { expected; actual }
    |> raise
