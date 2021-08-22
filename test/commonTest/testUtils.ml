open Format

open OUnit2

(* Testing Utilities *)

let not_equal ~ctxt msg pp expected actual =
  let msg = sprintf "%s are not equal" msg in
  let cmp _ _ = false in
  let printer v =
    fprintf str_formatter "%t" (pp v)
      |> flush_str_formatter
  in
  assert_equal ~ctxt ~cmp ~msg ~printer expected actual
