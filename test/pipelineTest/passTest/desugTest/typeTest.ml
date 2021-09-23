(* Types *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_desug_type_bool ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_bool env (fun env id ->
    let syntax = SyntaxTest.fresh_ty_constr ~id () in
    let annot = Annot.ty_bool in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_int ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_int env (fun env id ->
    let syntax = SyntaxTest.fresh_ty_constr ~id () in
    let annot = Annot.ty_int in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_long ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_long env (fun env id ->
    let syntax = SyntaxTest.fresh_ty_constr ~id () in
    let annot = Annot.ty_long in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_float ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_float env (fun env id ->
    let syntax = SyntaxTest.fresh_ty_constr ~id () in
    let annot = Annot.ty_float in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_double ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_double env (fun env id ->
    let syntax = SyntaxTest.fresh_ty_constr ~id () in
    let annot = Annot.ty_double in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_rune ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_rune env (fun env id ->
    let syntax = SyntaxTest.fresh_ty_constr ~id () in
    let annot = Annot.ty_rune in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_string ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_string env (fun env id ->
    let syntax = SyntaxTest.fresh_ty_constr ~id () in
    let annot = Annot.ty_string in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_unknown _ =
  let env = EnvTest.fresh () in
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen ~id:"UnknownConstructor" in
  let syntax = SyntaxTest.fresh_ty_constr ~loc ~id () in
  let exn = Desug.UnboundConstructor (loc, id) in
  assert_raises exn (fun _ ->
    Desug.desug_ty env syntax (fun _ ->
      assert_failure "Expected exception"))

(* Test Suite *)

let suite =
  "Types" >::: [
    "Boolean"             >:: test_desug_type_bool;
    "Integer"             >:: test_desug_type_int;
    "Long"                >:: test_desug_type_long;
    "Float"               >:: test_desug_type_float;
    "Double"              >:: test_desug_type_double;
    "Rune"                >:: test_desug_type_rune;
    "String"              >:: test_desug_type_string;
    "Unknown Constructor" >:: test_desug_type_unknown;
  ]
