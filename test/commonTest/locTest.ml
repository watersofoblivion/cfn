open Format

open OUnit2

open Common

(* Utilities *)

let dummy = Loc.loc Lexing.dummy_pos Lexing.dummy_pos

let pos = ref 0

let gen_pos fname =
  let lnum = !pos in
  incr pos;
  let bol = !pos in
  incr pos;
  let cnum = !pos in
  incr pos;
  { Lexing.pos_fname = fname;
    Lexing.pos_lnum  = lnum;
    Lexing.pos_bol   = bol;
    Lexing.pos_cnum  = cnum }

let gen _ =
  let fname = sprintf "file-%d.nile" !pos in
  incr pos;
  let start_pos = gen_pos fname in
  let end_pos = gen_pos fname in
  Loc.loc start_pos end_pos

let make (start_line, start_col, start_off) (end_line, end_col, end_off) =
  let start_pos =
    { Lexing.pos_fname = "";
      Lexing.pos_lnum  = start_line;
      Lexing.pos_bol   = start_off - start_col;
      Lexing.pos_cnum  = start_off; }
  in
  let end_pos =
    { Lexing.pos_fname = "";
      Lexing.pos_lnum  = end_line;
      Lexing.pos_bol   = end_off - end_col;
      Lexing.pos_cnum  = end_off; }
  in
  Loc.loc start_pos end_pos

let span src dst =
  make
    (src.Loc.start_pos.line, src.start_pos.col, src.start_pos.off)
    (dst.Loc.end_pos.line, dst.end_pos.col, dst.end_pos.off)

let span_from src dst =
  make
    src
    (dst.Loc.end_pos.line, dst.end_pos.col, dst.end_pos.off)

let shift (lines, cols, off) loc =
  make
    (loc.Loc.start_pos.line + lines, loc.start_pos.col + cols, loc.start_pos.off + off)
    (loc.end_pos.line + lines, loc.end_pos.col + cols, loc.end_pos.off + off)

(* Assertions *)

let assert_loc_elem ~ctxt elem expected actual =
  let msg = sprintf "Unexpected %s value" elem in
  assert_equal ~ctxt ~msg ~printer:string_of_int expected actual

let assert_loc_equal ~ctxt expected actual =
  assert_loc_elem ~ctxt "start line" expected.Loc.start_pos.line actual.Loc.start_pos.line;
  assert_loc_elem ~ctxt "start column" expected.start_pos.col actual.start_pos.col;
  assert_loc_elem ~ctxt "start offset" expected.start_pos.off actual.start_pos.off;

  assert_loc_elem ~ctxt "end line" expected.end_pos.line actual.end_pos.line;
  assert_loc_elem ~ctxt "end column" expected.end_pos.col actual.end_pos.col;
  assert_loc_elem ~ctxt "end offset" expected.end_pos.off actual.end_pos.off

(* Tests *)

let test_loc_loc ctxt =
  let fname = "ignored" in
  let start_pos = gen_pos fname in
  let end_pos = gen_pos fname in

  let loc = Loc.loc start_pos end_pos in

  let start_col = start_pos.pos_cnum - start_pos.pos_bol in
  assert_loc_elem ~ctxt "start line" start_pos.pos_lnum loc.start_pos.line;
  assert_loc_elem ~ctxt "start column" start_col loc.start_pos.col;
  assert_loc_elem ~ctxt "start offset" start_pos.pos_cnum loc.start_pos.off;

  let end_col = end_pos.pos_cnum - end_pos.pos_bol in
  assert_loc_elem ~ctxt "end line" end_pos.pos_lnum loc.end_pos.line;
  assert_loc_elem ~ctxt "end column" end_col loc.end_pos.col;
  assert_loc_elem ~ctxt "end offset" end_pos.pos_cnum loc.end_pos.off


let test_loc =
  "Constructor" >::: [
    "Spans Positions" >:: test_loc_loc;
  ]

(* Suite *)

let suite =
  "Location Tracking" >::: [
    test_loc;
  ]
