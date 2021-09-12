(* Type Checking *)

open Common

(* Exceptions *)

exception UnboundIdentifier of Sym.t
exception MismatchedTypes of Clos.ty * Clos.ty

let unbound_identifier id =
  UnboundIdentifier id
    |> raise

let mismatched_types inferred annotated =
  MismatchedTypes (inferred, annotated)
    |> raise
