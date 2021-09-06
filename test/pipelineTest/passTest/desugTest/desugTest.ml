open OUnit2

open Common

open CommonTest

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
    Syntax.expr_string loc [value]
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

let test_desug_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    LocTest.gen ()
      |> Syntax.patt_ground
  in
  let annot = Annot.patt_ground in
  Desug.desug_patt env syntax Annot.ty_bool (fun _ patt ->
    AnnotTest.assert_patt_equal ~ctxt annot patt)

let test_desug_patt_var ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    ()
      |> Sym.seq
      |> Sym.gen
      |> Syntax.patt_var loc
  in
  let id = () |> Sym.seq |> Sym.gen in
  let annot = Annot.patt_var id in
  let ty = Annot.ty_bool in
  Desug.desug_patt env syntax ty (fun env patt ->
    EnvTest.assert_bound ~ctxt AnnotTest.assert_ty_equal id env ty;
    AnnotTest.assert_patt_equal ~ctxt annot patt)

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

let assert_desug_top top ctxt =
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
      let value =
        let loc = LocTest.gen () in
        Syntax.expr_bool loc true
      in
      let binding =
        let loc = LocTest.gen () in
        Syntax.value_binding loc patt (Some ty) value
      in
      let loc = LocTest.gen () in
      top loc binding
    in
    let ty = Annot.ty_bool in
    let id = () |> Sym.seq |> Sym.gen in
    let annot =
      let patt = Annot.patt_var id in
      true
        |> Annot.expr_bool
        |> Annot.binding patt ty
        |> Annot.top_let
    in
    Desug.desug_top env syntax (fun env top ->
      EnvTest.assert_bound ~ctxt AnnotTest.assert_ty_equal id env ty;
      AnnotTest.assert_top_equal ~ctxt annot top))

let test_desug_top_let = assert_desug_top Syntax.top_let
let test_desug_top_val = assert_desug_top Syntax.top_val

let test_desug_file ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_bool env (fun env bool_constr ->
    Env.constr_of Prim.id_int env (fun env int_constr ->
      let syntax =
        let seq = Sym.seq () in
        let top =
          let patt =
            let loc = LocTest.gen () in
            seq
              |> Sym.gen
              |> Syntax.patt_var loc
          in
          let ty =
            let loc = LocTest.gen () in
            Syntax.ty_constr loc bool_constr
          in
          let value =
            let loc = LocTest.gen () in
            Syntax.expr_bool loc true
          in
          let binding =
            let loc = LocTest.gen () in
            Syntax.value_binding loc patt (Some ty) value
          in
          let loc = LocTest.gen () in
          Syntax.top_let loc binding
        in
        let top' =
          let patt =
            let loc = LocTest.gen () in
            seq
              |> Sym.gen
              |> Syntax.patt_var loc
          in
          let ty =
            let loc = LocTest.gen () in
            Syntax.ty_constr loc int_constr
          in
          let value =
            let loc = LocTest.gen () in
            Syntax.expr_int loc "42"
          in
          let binding =
            let loc = LocTest.gen () in
            Syntax.value_binding loc patt (Some ty) value
          in
          let loc = LocTest.gen () in
          Syntax.top_val loc binding
        in
        let pkg =
          let name =
            let loc = LocTest.gen () in
            seq
              |> Sym.gen
              |> Syntax.name loc
          in
          let loc = LocTest.gen () in
          Syntax.pkg loc name
        in
        Syntax.file pkg [] [top; top']
      in
      let seq = Sym.seq () in
      let id = Sym.gen seq in
      let ty = Annot.ty_bool in
      let id' = Sym.gen seq in
      let ty' = Annot.ty_int in
      let annot =
        let top =
          let patt = Annot.patt_var id in
          true
            |> Annot.expr_bool
            |> Annot.binding patt ty
            |> Annot.top_let
        in
        let top' =
          let patt = Annot.patt_var id' in
          42l
            |> Annot.expr_int
            |> Annot.binding patt ty'
            |> Annot.top_let
        in
        [top; top']
      in
      Desug.desug_file env syntax (fun env tops ->
        List.iter (fun (id, ty) ->
          EnvTest.assert_bound ~ctxt AnnotTest.assert_ty_equal id env ty
        ) [(id, ty); (id', ty')];
        List.iter2 (AnnotTest.assert_top_equal ~ctxt) annot tops)))

let suite =
  "Desugaring" >::: [
    "Types" >::: [
      "Boolean" >:: test_desug_type_bool;
      "Integer" >:: test_desug_type_int;
      "Long"    >:: test_desug_type_long;
      "Float"   >:: test_desug_type_float;
      "Double"  >:: test_desug_type_double;
      "Rune"    >:: test_desug_type_rune;
      "String"  >:: test_desug_type_string;
    ];
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
    "Patterns" >::: [
      "Ground"      >:: test_desug_patt_ground;
      "Identifiers" >:: test_desug_patt_var;
    ];
    "Bindings" >::: [
      "Value Bindings" >::: [
        "Implicit" >:: test_desug_binding_value_binding_implicit;
        "Explicit" >:: test_desug_binding_value_binding_explicit;
      ];
    ];
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_desug_top_let;
      "Val Bindings" >:: test_desug_top_val;
    ];
    "Files" >:: test_desug_file;
  ]
