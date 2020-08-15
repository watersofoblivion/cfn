(* open Format *)

open OUnit2

let success _ = ()

let string_equals ~ctxt expected actual =
  let printer s = s in
  let _ = assert_equal ~ctxt ~printer expected actual in
  actual

(*
module StringDiff = Simple_diff.Make(String)
let text_equals ~ctxt expected actual =
  let pp_diff fmt (expected, actual) =
    let changes_iter change = fprintf fmt "%s" change in
    let diff_iter change =
      let (prefix, changes) = match change with
        | Diff.Added lines -> ("+", lines)
        | Diff.Deleted lines -> ("-", lines)
        | Diff.Equal lines -> (" ", lines)
      in
      fprintf fmt "%s " prefix;
      List.iter changes_iter changes;
      fprintf fmt "\n"
    in

    let expected_lines = String.split_on_char '\n' expected in
    String.split_on_char '\n' actual
      |> Diff.get_diff expected_lines
      |> List.iter iter
  in
  let _ = assert_equal ~ctxt ~pp_diff expected actual in
  actual *)
