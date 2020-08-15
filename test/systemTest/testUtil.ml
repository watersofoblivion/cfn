open Format

open OUnit2

let with_env k v f x =
  let old =
    try Unix.getenv k
    with Not_found -> ""
  in
  let finally _ = Unix.putenv k old in
  let fn _ =
    Unix.putenv k v;
    f x
  in
  Fun.protect ~finally fn

let test_with_env =
  let var_name = "__TEST_VARIABLE__" in
  let external_value = "external-value" in
  let internal_value = "internal-value" in

  Unix.putenv var_name external_value;

  let test_with_env ctxt =
    let fn internal_value =
      try
        let actual = Sys.getenv var_name in
        assert_equal ~ctxt internal_value actual
      with Not_found ->
        assert_failure (sprintf "${%s} not set" var_name)
    in
    with_env var_name internal_value fn internal_value;

    try
      let actual = Sys.getenv var_name in
      assert_equal ~ctxt external_value actual
    with Not_found ->
      assert_failure (sprintf "${%s} not set" var_name)
  in
  "With Environment Variable" >:: test_with_env

let suite =
  "Test Utilities" >::: [
    test_with_env
  ]
