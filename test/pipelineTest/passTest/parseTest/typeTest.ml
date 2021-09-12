(* Types *)

open OUnit2

open Format

open Common

open CommonTest

(* Fixtures *)

let fresh_ty_constr ?start:(start = ParseUtils.bof) ?id:(id = SymTest.fresh_sym ~id:Prim.id_bool ()) _ =
  let loc = ParseUtils.sym_loc start id in
  SyntaxTest.fresh_ty_constr ~loc ~id ()

(* Assertions *)

let assert_parses_ty_annot = ParseUtils.assert_parses Parse.parse_annot SyntaxTest.assert_ty_equal
let assert_parses_ty = ParseUtils.assert_parses Parse.parse_ty SyntaxTest.assert_ty_equal

(* Tests *)

(* Types *)

let test_parse_ty_constr ctxt =
  let lexeme = "TestConstructor" in
  let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
  fresh_ty_constr ~id ()
    |> assert_parses_ty ~ctxt [lexeme]

(* Annotations *)

let test_parse_ty_annot ctxt =
  let lexeme = "TestConstructor" in
  let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
  fresh_ty_constr ~start:(1, 2, 2) ~id ()
    |> assert_parses_ty_annot ~ctxt [
         sprintf ": %s" lexeme
       ]

(* Test Suite *)

let suite =
  "Types" >::: [
    "Constructors" >:: test_parse_ty_constr;
    "Annotations"  >:: test_parse_ty_annot;
  ]
