open Common

module Menhir = Parser.MenhirInterpreter

include Lexer
include Parser

(* Tokenizers *)

let main_tokenizer = Sedlexing.with_tokenizer Lexer.lex_main
let import_path_tokenizer = Sedlexing.with_tokenizer Lexer.lex_import_path
let import_version_tokenizer = Sedlexing.with_tokenizer Lexer.lex_import_version
let rune_tokenizer = Sedlexing.with_tokenizer Lexer.lex_rune
let str_tokenizer = Sedlexing.with_tokenizer Lexer.lex_str

(* Higher-order Helpers *)

let input_needed tokenize parse lexbuf checkpoint =
  ()
    |> tokenize lexbuf
    |> Menhir.offer checkpoint
    |> parse lexbuf

let resume parse lexbuf checkpoint =
  checkpoint
    |> Menhir.resume
    |> parse lexbuf

(* Errors *)

exception ParseError of Loc.t
exception Rejected of string

let parse_error msg env = match Menhir.top env with
  | None ->
    Format.sprintf "Boom: %s parse_error" msg
      |> failwith
  | Some (Menhir.Element (_, _, start_loc, end_loc)) ->
    let loc = Loc.loc start_loc end_loc in
    Format.eprintf "%s parse error %@ (%d, %d, %d) - (%d, %d, %d)\n%!"
      msg
      start_loc.Lexing.pos_lnum
      (start_loc.Lexing.pos_cnum - start_loc.Lexing.pos_bol)
      start_loc.Lexing.pos_cnum
      end_loc.Lexing.pos_lnum
      (end_loc.Lexing.pos_cnum - end_loc.Lexing.pos_bol)
      end_loc.Lexing.pos_cnum
      ;
    ParseError loc
      |> raise

let reject msg =
  Rejected msg
    |> raise

(* Parser Drivers *)

let rec parse_main lexbuf checkpoint = match checkpoint with
  | Menhir.InputNeeded _ -> input_main lexbuf checkpoint
  | Menhir.Shifting (_, env, _) ->
    let resume = match Menhir.top env with
      | None -> failwith "Boom: parse_main no top"
      | Some (Menhir.Element (state, _, _, _)) ->
        match Menhir.incoming_symbol state with
          | Menhir.T Menhir.T_FROM -> resume_import_path
          | Menhir.T Menhir.T_SQUOTE -> resume_rune
          | Menhir.T Menhir.T_DQUOTE -> resume_str
          | Menhir.T _ -> resume_main
          | Menhir.N _ -> failwith "Expected non-terminal"
    in
    resume lexbuf checkpoint
  | Menhir.AboutToReduce _ -> resume_main lexbuf checkpoint
  | Menhir.HandlingError env -> parse_error "main" env
  | Menhir.Accepted value -> value
  | Menhir.Rejected -> reject "main"

and parse_import_path lexbuf checkpoint = match checkpoint with
  | Menhir.InputNeeded _ -> input_import_path lexbuf checkpoint
  | Menhir.Shifting (_, env, _) ->
    let resume = match Menhir.top env with
      | None -> failwith "Boom: parse_import_path no top"
      | Some (Menhir.Element (state, _, _, _)) ->
        match Menhir.incoming_symbol state with
          | Menhir.T Menhir.T_AT -> resume_import_version
          | Menhir.T Menhir.T_IMPORT -> resume_main
          | Menhir.T _ -> resume_import_path
          | Menhir.N _ -> failwith "Expected non-terminal"
    in
    resume lexbuf checkpoint
  | Menhir.AboutToReduce _ -> resume_import_path lexbuf checkpoint
  | Menhir.HandlingError env -> parse_error "import path" env
  | Menhir.Accepted value -> value
  | Menhir.Rejected -> reject "import path"

and parse_import_version lexbuf checkpoint = match checkpoint with
  | Menhir.InputNeeded _ -> input_import_version lexbuf checkpoint
  | Menhir.Shifting (_, env, _) ->
    let resume = match Menhir.top env with
      | None -> failwith "Boom: parse_import_version no top"
      | Some (Menhir.Element (state, _, _, _)) ->
        match Menhir.incoming_symbol state with
          | Menhir.T Menhir.T_IMPORT -> resume_main
          | Menhir.T _ -> resume_import_version
          | Menhir.N _ -> failwith "Expected non-terminal"
    in
    resume lexbuf checkpoint
  | Menhir.AboutToReduce _ -> resume_import_version lexbuf checkpoint
  | Menhir.HandlingError env -> parse_error "import version" env
  | Menhir.Accepted value -> value
  | Menhir.Rejected -> reject "import version"

and parse_rune lexbuf checkpoint = match checkpoint with
  | Menhir.InputNeeded _ -> input_rune lexbuf checkpoint
  | Menhir.Shifting (_, env, _) ->
    let resume = match Menhir.top env with
      | None -> failwith "Boom: parse_rune no top"
      | Some (Menhir.Element (state, _, _, _)) ->
        match Menhir.incoming_symbol state with
          | Menhir.T Menhir.T_SQUOTE -> resume_main
          | Menhir.T _ -> resume_rune
          | Menhir.N _ -> failwith "Expected non-terminal"
    in
    resume lexbuf checkpoint
  | Menhir.AboutToReduce _ -> resume_rune lexbuf checkpoint
  | Menhir.HandlingError env -> parse_error "rune" env
  | Menhir.Accepted value -> value
  | Menhir.Rejected -> reject "rune"

and parse_str lexbuf checkpoint = match checkpoint with
  | Menhir.InputNeeded _ -> input_str lexbuf checkpoint
  | Menhir.Shifting (_, env, _) ->
    let resume = match Menhir.top env with
      | None -> failwith "Boom: parse_str no top"
      | Some (Menhir.Element (state, _, _, _)) ->
        match Menhir.incoming_symbol state with
          | Menhir.T Menhir.T_DQUOTE -> resume_main
          | Menhir.T _ -> resume_str
          | Menhir.N _ -> failwith "Expected non-terminal"
    in
    resume lexbuf checkpoint
  | Menhir.AboutToReduce _ -> resume_str lexbuf checkpoint
  | Menhir.HandlingError env -> parse_error "str" env
  | Menhir.Accepted value -> value
  | Menhir.Rejected -> reject "str"

(* Input Helpers *)

and input_main lexbuf checkpoint = input_needed main_tokenizer parse_main lexbuf checkpoint
and input_import_path lexbuf checkpoint = input_needed import_path_tokenizer parse_import_path lexbuf checkpoint
and input_import_version lexbuf checkpoint = input_needed import_version_tokenizer parse_import_version lexbuf checkpoint
and input_rune lexbuf checkpoint = input_needed rune_tokenizer parse_rune lexbuf checkpoint
and input_str lexbuf checkpoint = input_needed str_tokenizer parse_str lexbuf checkpoint

(* Resume Helpers *)

and resume_main lexbuf checkpoint = resume parse_main lexbuf checkpoint
and resume_import_path lexbuf checkpoint = resume parse_import_path lexbuf checkpoint
and resume_import_version lexbuf checkpoint = resume parse_import_version lexbuf checkpoint
and resume_rune lexbuf checkpoint = resume parse_rune lexbuf checkpoint
and resume_str lexbuf checkpoint = resume parse_str lexbuf checkpoint

(* Source-File Parsers *)

let file_pos fname = {
  Lexing.pos_fname = fname;
  Lexing.pos_lnum = 1;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}

let parse_package_only path =
  let lexbuf = Lexer.lexbuf_from_file path in
  path
    |> file_pos
    |> Parser.Incremental.package_only
    |> parse_main lexbuf

let parse_imports_only path =
  let lexbuf = Lexer.lexbuf_from_file path in
  path
    |> file_pos
    |> Parser.Incremental.imports_only
    |> parse_main lexbuf

let parse_file path =
  let lexbuf = Lexer.lexbuf_from_file path in
  path
    |> file_pos
    |> Parser.Incremental.file
    |> parse_main lexbuf

(* Test Parsers *)

let start_pos = {
  Lexing.pos_fname = "-";
  Lexing.pos_lnum = 1;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}

let parse_annot lexbuf =
  start_pos
    |> Parser.Incremental.annot_test
    |> parse_main lexbuf

let parse_ty lexbuf =
  start_pos
    |> Parser.Incremental.ty_test
    |> parse_main lexbuf

let parse_un lexbuf =
  start_pos
    |> Parser.Incremental.un_test
    |> parse_main lexbuf

let parse_bin lexbuf =
  start_pos
    |> Parser.Incremental.bin_test
    |> parse_main lexbuf

let parse_patt lexbuf =
  start_pos
    |> Parser.Incremental.patt_test
    |> parse_main lexbuf

let parse_rune lexbuf =
  start_pos
    |> Parser.Incremental.rune_test
    |> parse_rune lexbuf

let parse_str lexbuf =
  start_pos
    |> Parser.Incremental.str_test
    |> parse_str lexbuf

let parse_lit lexbuf =
  start_pos
    |> Parser.Incremental.lit_test
    |> parse_main lexbuf

let parse_ident lexbuf =
  start_pos
    |> Parser.Incremental.ident_test
    |> parse_main lexbuf

let parse_atom lexbuf =
  start_pos
    |> Parser.Incremental.atom_test
    |> parse_main lexbuf

let parse_expr lexbuf =
  start_pos
    |> Parser.Incremental.expr_test
    |> parse_main lexbuf

let parse_term lexbuf =
  start_pos
    |> Parser.Incremental.term_test
    |> parse_main lexbuf

let parse_binding lexbuf =
  start_pos
    |> Parser.Incremental.binding_test
    |> parse_main lexbuf

let parse_top lexbuf =
  start_pos
    |> Parser.Incremental.top_test
    |> parse_main lexbuf

let parse_name lexbuf =
  start_pos
    |> Parser.Incremental.name_test
    |> parse_main lexbuf

let parse_pkgpath lexbuf =
  start_pos
    |> Parser.Incremental.pkgpath_test
    |> parse_main lexbuf

let parse_local lexbuf =
  start_pos
    |> Parser.Incremental.local_test
    |> parse_main lexbuf

let parse_host lexbuf =
  start_pos
    |> Parser.Incremental.host_test
    |> parse_import_path lexbuf

let parse_alias lexbuf =
  start_pos
    |> Parser.Incremental.alias_test
    |> parse_main lexbuf

let parse_pkgs lexbuf =
  start_pos
    |> Parser.Incremental.pkgs_test
    |> parse_main lexbuf

let parse_proto lexbuf =
  start_pos
    |> Parser.Incremental.proto_test
    |> parse_import_path lexbuf

let parse_hostpath lexbuf =
  start_pos
    |> Parser.Incremental.hostpath_test
    |> parse_import_path lexbuf

let parse_version lexbuf =
  start_pos
    |> Parser.Incremental.version_test
    |> parse_import_path lexbuf

let parse_src lexbuf =
  start_pos
    |> Parser.Incremental.src_test
    |> parse_import_path lexbuf

let parse_from lexbuf =
  start_pos
    |> Parser.Incremental.from_test
    |> parse_main lexbuf

let parse_import lexbuf =
  start_pos
    |> Parser.Incremental.import_test
    |> parse_main lexbuf

let parse_pkg lexbuf =
  start_pos
    |> Parser.Incremental.pkg_test
    |> parse_main lexbuf
