open Format

open OUnit2

open Common

(* Assertions *)

let assert_pp ~ctxt pp output value =
  let expected = String.concat "\n" output in
  fprintf str_formatter "%a" pp value
    |> flush_str_formatter
    |> assert_equal ~ctxt ~msg:"Unexpected pretty-printing output" ~printer:Fun.id expected

(* Tests *)

let test_ground ctxt =
  let pp fmt _ = Pretty.ground fmt in
  ()
    |> assert_pp ~ctxt pp ["_"]

let test_bool ctxt =
  true
    |> assert_pp ~ctxt Pretty.bool ["true"];
  false
    |> assert_pp ~ctxt Pretty.bool ["false"]

let test_int ctxt =
  42
    |> assert_pp ~ctxt Pretty.int ["42"]

(* Suite *)

let suite =
  "Pretty Printing" >::: [
    "Ground"                >:: test_ground;
    "Boolean"               >:: test_bool;
    "Integer"               >:: test_int;
  ]
