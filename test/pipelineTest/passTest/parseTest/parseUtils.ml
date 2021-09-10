open Common

open CommonTest

(* Fixtures *)

let bof = (0, 0, 0)

let loc_of_len (start_line, start_col, start_byte) cols bytes =
  LocTest.make (start_line, start_col, start_byte) (start_line, start_col + cols, start_byte + bytes)

let lexeme_loc start lexeme =
  let cols = Utf8.length lexeme in
  let bytes = String.length lexeme in
  loc_of_len start cols bytes

let sym_loc start sym = match Sym.id sym with
  | Some id -> lexeme_loc start id
  | None -> failwith "Symbol must have an identifier"

let fresh_of_lexeme ?start:(start = (0, 0, 0)) to_lexeme of_lexeme value =
  let lexeme = to_lexeme value in
  let loc = lexeme_loc start lexeme in
  of_lexeme loc lexeme

(* Assertions *)

let assert_parses ?env:(env = EnvTest.fresh ()) ~ctxt parse assert_equal input expected =
  let lexbuf =
    input
      |> String.concat "\n"
      |> Sedlexing.Utf8.from_string
  in
  ignore begin
    parse lexbuf env (fun _ actual ->
      assert_equal ~ctxt expected actual)
  end
