module Menhir = Parser.MenhirInterpreter

include Lexer
include Parser

let main_tokenizer = Sedlexing.with_tokenizer Lexer.lex_main

let rec parse_main lexbuf checkpoint = match checkpoint with
  | Menhir.InputNeeded _ ->
    ()
      |> main_tokenizer lexbuf
      |> Menhir.offer checkpoint
      |> parse_main lexbuf
  | Menhir.Shifting _ ->
    checkpoint
      |> Menhir.resume
      |> parse_main lexbuf
  | Menhir.AboutToReduce _ ->
    checkpoint
      |> Menhir.resume
      |> parse_main lexbuf
  | Menhir.HandlingError _ -> failwith "Parse error!"
  | Menhir.Accepted value -> value
  | Menhir.Rejected -> failwith "Rejected!"
(* and parse_rune lexbuf checkpoint = match checkpoint with
  | Menhir.InputNeeded env ->
  | Menhir.Shifting (before, after, more) ->
  | Menhir.AboutToReduce (env, prod) ->
  | Menhir.HandlingError env ->
  | Menhir.Accepted value ->
  | Menhir.Rejected -> *)
(* and parse_str_seg lexbuf checkpoint = match checkpoint with
  | Menhir.InputNeeded env ->
  | Menhir.Shifting (before, after, more) ->
  | Menhir.AboutToReduce (env, prod) ->
  | Menhir.HandlingError env ->
  | Menhir.Accepted value ->
  | Menhir.Rejected -> *)

let parse_package_only path =
  let lexbuf = Lexer.from_file path in
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.package_only
    |> parse_main lexbuf

let parse_imports_only path =
  let lexbuf = Lexer.from_file path in
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.imports_only
    |> parse_main lexbuf

let parse_file path =
  let lexbuf = Lexer.from_file path in
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.file
    |> parse_main lexbuf

let parse_annot lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.annot_test
    |> parse_main lexbuf

let parse_ty lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.ty_test
    |> parse_main lexbuf

let parse_lit lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.lit_test
    |> parse_main lexbuf

let parse_ident lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.ident_test
    |> parse_main lexbuf

let parse_expr lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.expr_test
    |> parse_main lexbuf

let parse_patt lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.patt_test
    |> parse_main lexbuf

let parse_binding lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.binding_test
    |> parse_main lexbuf

let parse_top lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.top_test
    |> parse_main lexbuf

let parse_name lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.name_test
    |> parse_main lexbuf

let parse_local lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.local_test
    |> parse_main lexbuf

let parse_alias lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.alias_test
    |> parse_main lexbuf

let parse_pkgs lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.pkgs_test
    |> parse_main lexbuf

let parse_src lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.src_test
    |> parse_main lexbuf

let parse_from lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.from_test
    |> parse_main lexbuf

let parse_import lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.import_test
    |> parse_main lexbuf

let parse_pkg lexbuf =
  let (start_pos, _) = Sedlexing.lexing_positions lexbuf in
  start_pos
    |> Parser.Incremental.pkg_test
    |> parse_main lexbuf
