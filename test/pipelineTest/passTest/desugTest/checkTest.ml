open OUnit2

let expected_exception _ _ = assert_failure "Expected exception"

let assert_raises_invalid_number_format loc lexeme constr msg fn =
  let exn = Desug.InvalidNumberFormat { loc; lexeme; constr; msg } in
  assert_raises exn (fun _ -> fn expected_exception)

let assert_raises_unbound_identifier loc id fn =
  let exn = Desug.UnboundIdentifier { loc; id } in
  assert_raises exn (fun _ -> fn expected_exception)

let assert_raises_mismatched_types loc inferred annotated fn =
  let exn = Desug.MismatchedTypes { loc; inferred; annotated } in
  assert_raises exn (fun _ -> fn expected_exception)

let assert_raises_unsupported_bin_op_promotion loc op sub sup fn =
  let exn = Desug.UnsupportedBinOpPromotion { loc; op; sub; sup } in
  assert_raises exn (fun _ -> fn expected_exception)

let assert_raises_invalid_codepoint loc lexeme codepoint fn =
  let exn = Desug.InvalidCodepoint { loc; lexeme; codepoint } in
  assert_raises exn (fun _ -> fn expected_exception)

let assert_raises_unbound_constructor loc id fn =
  let exn = Desug.UnboundConstructor { loc; id } in
  assert_raises exn (fun _ -> fn expected_exception)
