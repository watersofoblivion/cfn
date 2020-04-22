open Format

open OUnit2

open Syntax

(* Mocks *)

let mock_lexbuf fname (start_lnum, start_bol, start_cnum) (end_lnum, end_bol, end_cnum) =
  let pos lnum bol cnum =
    { Lexing.pos_fname = fname;
      pos_lnum         = lnum;
      pos_bol          = bol;
      pos_cnum         = cnum }
  in
  let lexbuf = Lexing.from_string "" in
  lexbuf.lex_start_p <- pos start_lnum start_bol start_cnum ;
  lexbuf.lex_curr_p <- pos end_lnum end_bol end_cnum ;
  lexbuf

(* Assertions *)

let assert_loc ~ctxt loc fname (start_line, start_col, start_off) (end_line, end_col, end_off) len =
  assert_equal ~ctxt fname loc.Loc.fname;
  assert_equal ~ctxt start_line loc.start_pos.line;
  assert_equal ~ctxt start_col loc.start_pos.col;
  assert_equal ~ctxt start_off loc.start_pos.off;
  assert_equal ~ctxt end_line loc.end_pos.line;
  assert_equal ~ctxt end_col loc.end_pos.col;
  assert_equal ~ctxt end_off loc.end_pos.off;
  assert_equal ~ctxt len loc.length

(* Location Tracking *)

(* Dummy Location *)
let test_dummy_loc ctxt =
  let loc = Loc.dummy in
  assert_loc ~ctxt loc "" (-1, -1, -1) (-1, -1, -1) (-1)

(* Mock Location *)
let test_mock_loc ctxt =
  let fname = "filename.cfn" in
  let start_line = 1 in
  let start_col = 2 in
  let start_off = 3 in
  let end_line = 4 in
  let end_col = 5 in
  let end_off = 6 in

  let loc = Loc.mock fname (start_line, start_col, start_off) (end_line, end_col, end_off) in
  assert_loc ~ctxt loc fname (start_line, start_col, start_off) (end_line, end_col, end_off) 3

(* Location constructor *)
let test_loc ctxt =
  let fname = "test-filename.cfn" in
  let lexbuf = mock_lexbuf fname (1, 10, 15) (1, 10, 20) in

  let loc = Loc.loc lexbuf in
  assert_loc ~ctxt loc fname (1, 5, 15) (1, 10, 20) 5

(* Location spanning *)
let test_span =
  let test_span ctxt =
    let fname = "test-filename.cfn" in

    let lexbuf = mock_lexbuf fname (1, 10, 15) (1, 10, 20) in
    let start_loc = Loc.loc lexbuf in

    let lexbuf = mock_lexbuf fname (1, 10, 15) (3, 100, 105) in
    let end_loc = Loc.loc lexbuf in

    let loc = Loc.span start_loc end_loc in
    assert_loc ~ctxt loc fname (1, 5, 15) (3, 5, 105) 90
  in
  let test_dummy =
    let real = Loc.mock "" (1, 2, 3) (4, 5, 6) in

    let test_left ctxt =
      let actual = Loc.span Loc.dummy real in
      assert_equal ~ctxt (-1) actual.length
    in
    let test_right ctxt =
      let actual = Loc.span real Loc.dummy in
      assert_equal ~ctxt (-1) actual.length
    in
    let test_both ctxt =
      let actual = Loc.span Loc.dummy Loc.dummy in
      assert_equal ~ctxt (-1) actual.length
    in
    "Dummy Locations" >::: [
      "Left"  >:: test_left;
      "Right" >:: test_right;
      "Both"  >:: test_both
    ]
  in
  let test_mismatched_filenames _ =
    let fname_1 = "file-1.cfn" in
    let lexbuf = mock_lexbuf fname_1 (1, 10, 15) (1, 10, 20) in
    let start_loc = Loc.loc lexbuf in

    let fname_2 = "file-2.cfn" in
    let lexbuf = mock_lexbuf fname_2 (1, 10, 15) (1, 10, 20) in
    let end_loc = Loc.loc lexbuf in

    let msg =
      sprintf "Locations are from different files: %S and %S" fname_1 fname_2
    in
    let exn = Failure msg in
    let fn _ =
      let _ = Loc.span start_loc end_loc in
      ()
    in
    assert_raises exn fn
  in
  "Merging Locations" >::: [
    "Spanning Locations"        >:: test_span;
    test_dummy;
    "With Mismatched Filenames" >:: test_mismatched_filenames
  ]

let suite =
  "Location Tracking" >::: [
    "Dummy Location"     >:: test_dummy_loc;
    "Mock Location"      >:: test_mock_loc;
    "From Lexing Buffer" >:: test_loc;
    test_span
  ]
