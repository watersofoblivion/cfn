(* Types *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_desug_type_bool ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_bool env (fun env sym ->
    let syntax = Syntax.ty_constr LocTest.dummy sym in
    let annot = Annot.ty_bool in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_int ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_int env (fun env sym ->
    let syntax = Syntax.ty_constr LocTest.dummy sym in
    let annot = Annot.ty_int in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_long ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_long env (fun env sym ->
    let syntax = Syntax.ty_constr LocTest.dummy sym in
    let annot = Annot.ty_long in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_float ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_float env (fun env sym ->
    let syntax = Syntax.ty_constr LocTest.dummy sym in
    let annot = Annot.ty_float in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_double ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_double env (fun env sym ->
    let syntax = Syntax.ty_constr LocTest.dummy sym in
    let annot = Annot.ty_double in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_rune ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_rune env (fun env sym ->
    let syntax = Syntax.ty_constr LocTest.dummy sym in
    let annot = Annot.ty_rune in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_string ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_string env (fun env sym ->
    let syntax = Syntax.ty_constr LocTest.dummy sym in
    let annot = Annot.ty_string in
    AnnotTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

(* Test Suite *)

let suite =
  "Types" >::: [
    "Boolean" >:: test_desug_type_bool;
    "Integer" >:: test_desug_type_int;
    "Long"    >:: test_desug_type_long;
    "Float"   >:: test_desug_type_float;
    "Double"  >:: test_desug_type_double;
    "Rune"    >:: test_desug_type_rune;
    "String"  >:: test_desug_type_string;
  ]
