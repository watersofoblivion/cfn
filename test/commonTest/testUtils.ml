open Format

open OUnit2

(* Testing Utilities *)

let not_equal ~ctxt msg pp expected actual =
  let msg = sprintf "%s are not equal" msg in
  let cmp _ _ = false in
  let printer v =
    fprintf str_formatter "%a" pp v
      |> flush_str_formatter
  in
  assert_equal ~ctxt ~cmp ~msg ~printer expected actual

let assert_optional_equal ~ctxt id assert_equal expected actual = match (expected, actual) with
  | Some expected, Some actual -> assert_equal ~ctxt expected actual
  | None, None -> ()
  | Some _, None ->
    id
      |> sprintf "Expected %s to be present"
      |> assert_failure
  | None, Some _ ->
    id
      |> sprintf "Unexpected %s present"
      |> assert_failure
