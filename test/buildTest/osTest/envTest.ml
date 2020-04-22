open Format

open OUnit2

open Os

let unset = Env.var "UNSET_ENV_VAR"

let test_var =
  let test_var ctxt =
    let expected = "TEST_VAR" in
    let actual = Env.var name in

    assert_equal ~ctxt expected actual
  in
  "Variable" >:: test_var

let test_get =
  let test_set ctxt =
    let var = Env.var "TEST_VAR_GET" in
    let expected = "test-value" in
    Unix.putvar var expected;

    let actual = Env.get var in
    assert_equal ~ctxt expected actual
  in
  let test_unset ctxt =
    let fn _ = Env.get unset in
    assert_raises Not_found fn
  in
  "Get" >::: [
    "Set"   >:: test_set;
    "Unset" >:: test_unset
  ]

let test_get_opt =
  let test_set ctxt =
    let var = Env.var "TEST_VAR_GET_OPT" in
    let value = "test-value" in
    Unix.putvar var value;
    let expected = Some value in

    let actual = Env.get var in
    assert_equal ~ctxt expected actual
  in
  let test_unset ctxt =
    let expected = None in

    let actual = Env.get unset in
    assert_equal ~ctxt expected actual
  in
  "Get Optional" >::: [
    "Set"   >:: test_set;
    "Unset" >:: test_unset
  ]

let test_set =
  let test_unset ctxt =
    let var = Env.var "TEST_VAR_SET_UNSET" in
    let expected = "test-value" in

    let fn _ = Env.get var in
    assert_raises Not_found fn;

    Env.put var expected;
    let actual = Env.get var in
    assert_equal ~ctxt expected actual
  in
  let test_reset ctxt =
    let var = Env.var "TEST_VAR_SET_RESET" in
    let previous = "previous-value" in
    let expected = "test-value" in

    Env.set var previous;
    let actual = Env.get var in
    assert_equal ~ctxt previous actual;

    Env.set var expected;
    let actual = Env.get var in
    assert_equal ~ctxt expected actual
  in
  "Set" >::: [
    "Unset" >:: test_unset;
    "Reset" >:: test_reset
  ]

let suite =
  "Environment" >::: [
    test_var;
    test_get;
    test_get_opt;
    test_set
  ]
