(* Abstract Syntax *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_top_let ?start:(start = ParseUtils.bof) ?binding:(binding = ExprTest.fresh_value_binding ()) _ =
  let loc =
    binding
      |> Syntax.loc_binding
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_top_let ~loc ~binding ()

let fresh_top_val ?start:(start = ParseUtils.bof) ?binding:(binding = ExprTest.fresh_value_binding ()) _ =
  let loc =
    binding
      |> Syntax.loc_binding
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_top_val ~loc ~binding ()

(* Assertions *)

let assert_parses_top = ParseUtils.assert_parses Parse.parse_top SyntaxTest.assert_top_equal

(* Tests *)

let test_parse_top_let ctxt =
  let id = "testId" in
  let seq = Sym.seq () in
  let binding =
    let id = Sym.gen seq ~id in
    let patt = PattTest.fresh_patt_var ~start:(1, 4, 4) ~id () in
    let ty =
      let id = Sym.gen seq ~id:Prim.id_bool in
      TypeTest.fresh_ty_constr ~start:(1, 12, 12) ~id ()
    in
    let value = ExprTest.fresh_atom_bool ~start:(1, 19, 19) ~value:true () in
    ExprTest.fresh_value_binding ~patt ~explicit:true ~ty ~value ()
  in
  fresh_top_let ~binding ()
    |> assert_parses_top ~ctxt [sprintf "let %s: %s = true" id Prim.id_bool]

let test_parse_top_val ctxt =
  let id = "testId" in
  let seq = Sym.seq () in
  let binding =
    let id = Sym.gen seq ~id in
    let patt = PattTest.fresh_patt_var ~start:(1, 4, 4) ~id () in
    let ty =
      let id = Sym.gen seq ~id:Prim.id_bool in
      TypeTest.fresh_ty_constr ~start:(1, 12, 12) ~id ()
    in
    let value = ExprTest.fresh_atom_bool ~start:(1, 19, 19) ~value:true () in
    ExprTest.fresh_value_binding ~patt ~explicit:true ~ty ~value ()
  in
  fresh_top_val ~binding ()
    |> assert_parses_top ~ctxt [sprintf "val %s: %s = true" id Prim.id_bool]

(* Test Suite *)

let suite =
  "Top-Level Expressions" >::: [
    "Let Binding"   >:: test_parse_top_let;
    "Value Binding" >:: test_parse_top_val;
  ]
