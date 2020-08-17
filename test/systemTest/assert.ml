open Format

open OUnit2

(* Generic *)

let should matcher extractor ~ctxt expected value =
  let _ =
    value
      |> extractor
      |> matcher ~ctxt expected
  in
  value

let success _ = ()

(* Strings *)

let string_equals ~ctxt ?msg:(msg = "") expected actual =
  let printer s = s in
  let _ = assert_equal ~ctxt ~msg ~printer expected actual in
  actual

let text_equals ~ctxt ?msg:(msg = "") expected actual =
  let module StringDiff = Simple_diff.Make(String) in
  let pp_diff fmt (expected, actual) =
    let changes_iter style change =
      let change = ANSITerminal.sprintf style "%s" change in
      fprintf fmt "%s" change
    in
    let diff_iter change =
      let (prefix, changes, style) = match change with
        | StringDiff.Added lines -> ("+", lines, [ANSITerminal.green])
        | StringDiff.Deleted lines -> ("-", lines, [ANSITerminal.red])
        | StringDiff.Equal lines -> (" ", lines, [])
      in

      let prefix = ANSITerminal.sprintf style "%s" prefix in
      fprintf fmt "%s " prefix;
      Array.iter (changes_iter style) changes;
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
  let _ = assert_equal ~ctxt ~msg ~pp_diff expected actual in
  actual

(* Exceptions *)

let fails_with msg fn =
  let exn = Failure msg in
  assert_raises exn fn

let invalid_argument msg fn =
  let exn = Invalid_argument msg in
  assert_raises exn fn
