open Common

(* Assertions *)

let assert_parses ~ctxt lex parse assert_equal env input expected =
  let lexbuf =
    input
      |> String.concat "\n"
      |> Lex.from_string
  in
  ignore begin
    parse lex lexbuf env (fun _ actual ->
      assert_equal ~ctxt expected actual)
  end
