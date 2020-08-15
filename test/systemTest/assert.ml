open Format

open OUnit2

let success _ = ()

let string_equals ~ctxt expected actual =
  let printer s = s in
  let _ = assert_equal ~ctxt ~printer expected actual in
  actual

let text_equals ~ctxt expected actual =
  let module StringDiff = Simple_diff.Make(String) in
  let pp_diff fmt (expected, actual) =
    let changes_iter change = fprintf fmt "%s" change in
    let diff_iter change =
      let (prefix, changes) = match change with
        | StringDiff.Added lines -> ("+", lines)
        | StringDiff.Deleted lines -> ("-", lines)
        | StringDiff.Equal lines -> (" ", lines)
      in
      fprintf fmt "%s " prefix;
      Array.iter changes_iter changes;
      fprintf fmt "\n"
    in
    let explode str =
      str
        |> String.split_on_char '\n'
        |> Array.of_list
    in
    fprintf fmt "\n";
    let expected_lines = explode expected in
    actual
      |> explode
      |> StringDiff.get_diff expected_lines
      |> List.iter diff_iter
  in
  let _ = assert_equal ~ctxt ~pp_diff expected actual in
  actual
