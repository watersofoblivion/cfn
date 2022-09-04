open OUnit2

(* Assertions *)

let expected_exception _ _ = assert_failure "Expected exception"

let assert_raises_unbound_identifier id fn =
  let exn = Norm.UnboundIdentifier { id } in
  assert_raises exn (fun _ -> fn expected_exception)

let assert_raises_mismatched_types inferred annotated fn =
  let exn = Norm.MismatchedTypes { inferred; annotated } in
  assert_raises exn (fun _ -> fn expected_exception)
