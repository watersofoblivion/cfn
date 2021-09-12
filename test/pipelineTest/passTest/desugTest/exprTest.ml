(* Abstract Syntax *)

open OUnit2

open Common

open CommonTest

(* Tests *)

(* Expressions *)

let test_desug_expr_bool ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.expr_bool loc true
  in
  let annot = Annot.expr_bool true in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.assert_ty_equal ~ctxt Annot.ty_bool ty;
    AnnotTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_int ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.expr_int loc "+42i"
  in
  let annot = Annot.expr_int 42l in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.assert_ty_equal ~ctxt Annot.ty_int ty;
    AnnotTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_int_invalid _ =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_int env (fun env sym ->
    let loc = LocTest.gen () in
    let lexeme = "not a valid number" in
    let syntax = Syntax.expr_int loc lexeme in
    let exn = Desug.InvalidNumberFormat (loc, lexeme, sym, "Int32.of_string") in
    assert_raises exn (fun _ ->
      Desug.desug_expr env syntax (fun _ _ ->
        assert_failure "Expected exception")))

let test_desug_expr_long ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.expr_long loc "+42L"
  in
  let annot = Annot.expr_long 42L in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.assert_ty_equal ~ctxt Annot.ty_long ty;
    AnnotTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_long_invalid _ =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_long env (fun env sym ->
    let loc = LocTest.gen () in
    let lexeme = "not a valid number" in
    let syntax = Syntax.expr_long loc lexeme in
    let exn = Desug.InvalidNumberFormat (loc, lexeme, sym, "Int64.of_string") in
    assert_raises exn (fun _ ->
      Desug.desug_expr env syntax (fun _ _ ->
        assert_failure "Expected exception")))

let test_desug_expr_float ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.expr_float loc "+4.2f"
  in
  let annot = Annot.expr_float 4.2 in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.assert_ty_equal ~ctxt Annot.ty_float ty;
    AnnotTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_float_invalid _ =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_float env (fun env sym ->
    let loc = LocTest.gen () in
    let lexeme = "not a valid number" in
    let syntax = Syntax.expr_float loc lexeme in
    let exn = Desug.InvalidNumberFormat (loc, lexeme, sym, "float_of_string") in
    assert_raises exn (fun _ ->
      Desug.desug_expr env syntax (fun _ _ ->
        assert_failure "Expected exception")))

let test_desug_expr_double ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.expr_double loc "+4.2D"
  in
  let annot = Annot.expr_double 4.2 in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.assert_ty_equal ~ctxt Annot.ty_double ty;
    AnnotTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_double_invalid _ =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_double env (fun env sym ->
    let loc = LocTest.gen () in
    let lexeme = "not a valid number" in
    let syntax = Syntax.expr_double loc lexeme in
    let exn = Desug.InvalidNumberFormat (loc, lexeme, sym, "float_of_string") in
    assert_raises exn (fun _ ->
      Desug.desug_expr env syntax (fun _ _ ->
        assert_failure "Expected exception")))

let test_desug_expr_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let syntax =
    let value =
      let loc = LocTest.gen () in
      Syntax.rune_lit loc value
    in
    let loc = LocTest.gen () in
    Syntax.expr_rune loc value
  in
  let annot = Annot.expr_rune value in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.assert_ty_equal ~ctxt Annot.ty_rune ty;
    AnnotTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_string ctxt =
  let env = EnvTest.fresh () in
  let value = "foo bar" in
  let syntax =
    let value =
      let loc = LocTest.gen () in
      Syntax.str_lit loc value
    in
    let loc = LocTest.gen () in
    Syntax.expr_string loc [[value]]
  in
  let annot = Annot.expr_string value in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.assert_ty_equal ~ctxt Annot.ty_string ty;
    AnnotTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_ident ctxt =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Annot.ty_bool in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.expr_ident loc value
  in
  let annot = Annot.expr_ident value in
  Env.bind value bound env (fun env ->
    Desug.desug_expr env syntax (fun ty expr ->
      AnnotTest.assert_ty_equal ~ctxt bound ty;
      AnnotTest.assert_expr_equal ~ctxt annot expr))

let test_desug_expr_ident_unbound _ =
  let env = EnvTest.fresh () in
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  let syntax = Syntax.expr_ident loc id in
  let exn = Desug.UnboundIdentifier (loc, id) in
  assert_raises exn (fun _ ->
    Desug.desug_expr env syntax (fun _ _ ->
      assert_failure "Expected exception"))

(* Bindings *)

let test_desug_binding_value_binding_implicit ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let patt =
      let loc = LocTest.gen () in
      ()
        |> Sym.seq
        |> Sym.gen
        |> Syntax.patt_var loc
    in
    let loc = LocTest.gen () in
    true
      |> Syntax.expr_bool loc
      |> Syntax.value_binding loc patt None
  in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Annot.ty_bool in
  let annot =
    let patt = Annot.patt_var id in
    true
      |> Annot.expr_bool
      |> Annot.binding patt ty
  in
  Desug.desug_binding env syntax (fun env binding ->
    EnvTest.assert_bound ~ctxt AnnotTest.assert_ty_equal id env ty;
    AnnotTest.assert_binding_equal ~ctxt annot binding)

let test_desug_binding_value_binding_explicit ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_bool env (fun env sym ->
    let syntax =
      let patt =
        let loc = LocTest.gen () in
        ()
          |> Sym.seq
          |> Sym.gen
          |> Syntax.patt_var loc
      in
      let ty =
        let loc = LocTest.gen () in
        Syntax.ty_constr loc sym
      in
      let loc = LocTest.gen () in
      true
        |> Syntax.expr_bool loc
        |> Syntax.value_binding loc patt (Some ty)
    in
    let id = () |> Sym.seq |> Sym.gen in
    let ty = Annot.ty_bool in
    let annot =
      let patt = Annot.patt_var id in
      true
        |> Annot.expr_bool
        |> Annot.binding patt ty
    in
    Desug.desug_binding env syntax (fun env binding ->
      EnvTest.assert_bound ~ctxt AnnotTest.assert_ty_equal id env ty;
      AnnotTest.assert_binding_equal ~ctxt annot binding))

(* Test Suite *)

let suite =
  "Abstract Syntax" >::: [
    "Expressions" >::: [
      "Booleans" >:: test_desug_expr_bool;
      "Integers" >::: [
        "Valid"   >:: test_desug_expr_int;
        "Invalid" >:: test_desug_expr_int_invalid;
      ];
      "Longs" >::: [
        "Valid"   >:: test_desug_expr_long;
        "Invalid" >:: test_desug_expr_long_invalid;
      ];
      "Floats" >::: [
        "Valid"   >:: test_desug_expr_float;
        "Invalid" >:: test_desug_expr_float_invalid;
      ];
      "Doubles" >::: [
        "Valid"   >:: test_desug_expr_double;
        "Invalid" >:: test_desug_expr_double_invalid;
      ];
      "Runes"    >:: test_desug_expr_rune;
      "Strings"  >:: test_desug_expr_string;
      "Identifiers" >::: [
        "Bound"   >:: test_desug_expr_ident;
        "Unbound" >:: test_desug_expr_ident_unbound;
      ]
    ];
    "Bindings" >::: [
      "Value Bindings" >::: [
        "Implicit" >:: test_desug_binding_value_binding_implicit;
        "Explicit" >:: test_desug_binding_value_binding_explicit;
      ];
    ];
  ]
