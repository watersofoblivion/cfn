open OUnit2

(* Assertions *)

let expected_exception _ _ = assert_failure "Expected exception"

let assert_raises_unbound_identifier id fn =
  let exn = Conv.UnboundIdentifier { id } in
  assert_raises exn (fun _ -> fn expected_exception)

let assert_raises_invalid_arity expected actual fn =
  let exn = Conv.InvalidArity { expected; actual } in
  assert_raises exn (fun _ -> fn expected_exception)

let assert_raises_mismatched_types inferred annotated fn =
  let exn = Conv.MismatchedTypes { inferred; annotated } in
  assert_raises exn (fun _ -> fn expected_exception)
