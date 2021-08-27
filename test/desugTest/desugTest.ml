open OUnit2

open Common

open CommonTest

let test_desug_type_bool ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_bool env (fun env sym ->
    let syntax = Syntax.Type.constr LocTest.dummy sym in
    let annot = Annot.Type.bool in
    AnnotTest.TypeTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_int ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_int env (fun env sym ->
    let syntax = Syntax.Type.constr LocTest.dummy sym in
    let annot = Annot.Type.int in
    AnnotTest.TypeTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_long ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_long env (fun env sym ->
    let syntax = Syntax.Type.constr LocTest.dummy sym in
    let annot = Annot.Type.long in
    AnnotTest.TypeTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_float ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_float env (fun env sym ->
    let syntax = Syntax.Type.constr LocTest.dummy sym in
    let annot = Annot.Type.float in
    AnnotTest.TypeTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_double ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_double env (fun env sym ->
    let syntax = Syntax.Type.constr LocTest.dummy sym in
    let annot = Annot.Type.double in
    AnnotTest.TypeTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_rune ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_rune env (fun env sym ->
    let syntax = Syntax.Type.constr LocTest.dummy sym in
    let annot = Annot.Type.rune in
    AnnotTest.TypeTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_type_string ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_string env (fun env sym ->
    let syntax = Syntax.Type.constr LocTest.dummy sym in
    let annot = Annot.Type.string in
    AnnotTest.TypeTest.assert_ty_equal ~ctxt annot
      |> Desug.desug_ty env syntax)

let test_desug_expr_bool ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.Ast.bool loc true
  in
  let annot = Annot.Ast.bool true in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.TypeTest.assert_ty_equal ~ctxt Annot.Type.bool ty;
    AnnotTest.AstTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_int ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.Ast.int loc "+42i"
  in
  let annot = Annot.Ast.int 42l in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.TypeTest.assert_ty_equal ~ctxt Annot.Type.int ty;
    AnnotTest.AstTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_int_invalid _ =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_int env (fun env sym ->
    let loc = LocTest.gen () in
    let lexeme = "not a valid number" in
    let syntax = Syntax.Ast.int loc lexeme in
    let exn = Desug.InvalidNumberFormat (loc, lexeme, sym, "Int32.of_string") in
    assert_raises exn (fun _ ->
      Desug.desug_expr env syntax (fun _ _ ->
        assert_failure "Expected exception")))

let test_desug_expr_long ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.Ast.long loc "+42L"
  in
  let annot = Annot.Ast.long 42L in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.TypeTest.assert_ty_equal ~ctxt Annot.Type.long ty;
    AnnotTest.AstTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_long_invalid _ =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_long env (fun env sym ->
    let loc = LocTest.gen () in
    let lexeme = "not a valid number" in
    let syntax = Syntax.Ast.long loc lexeme in
    let exn = Desug.InvalidNumberFormat (loc, lexeme, sym, "Int64.of_string") in
    assert_raises exn (fun _ ->
      Desug.desug_expr env syntax (fun _ _ ->
        assert_failure "Expected exception")))

let test_desug_expr_float ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.Ast.float loc "+4.2f"
  in
  let annot = Annot.Ast.float 4.2 in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.TypeTest.assert_ty_equal ~ctxt Annot.Type.float ty;
    AnnotTest.AstTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_float_invalid _ =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_float env (fun env sym ->
    let loc = LocTest.gen () in
    let lexeme = "not a valid number" in
    let syntax = Syntax.Ast.float loc lexeme in
    let exn = Desug.InvalidNumberFormat (loc, lexeme, sym, "float_of_string") in
    assert_raises exn (fun _ ->
      Desug.desug_expr env syntax (fun _ _ ->
        assert_failure "Expected exception")))

let test_desug_expr_double ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.Ast.double loc "+4.2D"
  in
  let annot = Annot.Ast.double 4.2 in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.TypeTest.assert_ty_equal ~ctxt Annot.Type.double ty;
    AnnotTest.AstTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_double_invalid _ =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_double env (fun env sym ->
    let loc = LocTest.gen () in
    let lexeme = "not a valid number" in
    let syntax = Syntax.Ast.double loc lexeme in
    let exn = Desug.InvalidNumberFormat (loc, lexeme, sym, "float_of_string") in
    assert_raises exn (fun _ ->
      Desug.desug_expr env syntax (fun _ _ ->
        assert_failure "Expected exception")))

let test_desug_expr_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.Ast.rune loc value
  in
  let annot = Annot.Ast.rune value in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.TypeTest.assert_ty_equal ~ctxt Annot.Type.rune ty;
    AnnotTest.AstTest.assert_expr_equal ~ctxt annot expr)

let test_desug_expr_string ctxt =
  let env = EnvTest.fresh () in
  let value =
    "foo bar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
  in
  let syntax =
    let loc = LocTest.gen () in
    Syntax.Ast.string loc value
  in
  let annot = Annot.Ast.string value in
  Desug.desug_expr env syntax (fun ty expr ->
    AnnotTest.TypeTest.assert_ty_equal ~ctxt Annot.Type.string ty;
    AnnotTest.AstTest.assert_expr_equal ~ctxt annot expr)

let test_desug_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    LocTest.gen ()
      |> Syntax.Ast.patt_ground
  in
  let annot = Annot.Ast.patt_ground in
  Desug.desug_patt env syntax Annot.Type.bool (fun _ patt ->
    AnnotTest.AstTest.assert_patt_equal ~ctxt annot patt)

let test_desug_patt_var ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let loc = LocTest.gen () in
    ()
      |> Sym.seq
      |> Sym.gen
      |> Syntax.Ast.patt_var loc
  in
  let id = () |> Sym.seq |> Sym.gen in
  let annot = Annot.Ast.patt_var id in
  let ty = Annot.Type.bool in
  Desug.desug_patt env syntax ty (fun env patt ->
    EnvTest.assert_bound ~ctxt AnnotTest.TypeTest.assert_ty_equal id env ty;
    AnnotTest.AstTest.assert_patt_equal ~ctxt annot patt)

let test_desug_binding_value_binding_implicit ctxt =
  let env = EnvTest.fresh () in
  let syntax =
    let patt =
      let loc = LocTest.gen () in
      ()
        |> Sym.seq
        |> Sym.gen
        |> Syntax.Ast.patt_var loc
    in
    let loc = LocTest.gen () in
    true
      |> Syntax.Ast.bool loc
      |> Syntax.Ast.value_binding loc patt None
  in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Annot.Type.bool in
  let annot =
    let patt = Annot.Ast.patt_var id in
    true
      |> Annot.Ast.bool
      |> Annot.Ast.binding patt ty
  in
  Desug.desug_binding env syntax (fun env binding ->
    EnvTest.assert_bound ~ctxt AnnotTest.TypeTest.assert_ty_equal id env ty;
    AnnotTest.AstTest.assert_binding_equal ~ctxt annot binding)

let test_desug_binding_value_binding_explicit ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_bool env (fun env sym ->
    let syntax =
      let patt =
        let loc = LocTest.gen () in
        ()
          |> Sym.seq
          |> Sym.gen
          |> Syntax.Ast.patt_var loc
      in
      let ty =
        let loc = LocTest.gen () in
        Syntax.Type.constr loc sym
      in
      let loc = LocTest.gen () in
      true
        |> Syntax.Ast.bool loc
        |> Syntax.Ast.value_binding loc patt (Some ty)
    in
    let id = () |> Sym.seq |> Sym.gen in
    let ty = Annot.Type.bool in
    let annot =
      let patt = Annot.Ast.patt_var id in
      true
        |> Annot.Ast.bool
        |> Annot.Ast.binding patt ty
    in
    Desug.desug_binding env syntax (fun env binding ->
      EnvTest.assert_bound ~ctxt AnnotTest.TypeTest.assert_ty_equal id env ty;
      AnnotTest.AstTest.assert_binding_equal ~ctxt annot binding))

let assert_desug_top top ctxt =
  let env = EnvTest.fresh () in
  Env.constr_of Prim.id_bool env (fun env sym ->
    let syntax =
      let patt =
        let loc = LocTest.gen () in
        ()
          |> Sym.seq
          |> Sym.gen
          |> Syntax.Ast.patt_var loc
      in
      let ty =
        let loc = LocTest.gen () in
        Syntax.Type.constr loc sym
      in
      let value =
        let loc = LocTest.gen () in
        Syntax.Ast.bool loc true
      in
      let binding =
        let loc = LocTest.gen () in
        Syntax.Ast.value_binding loc patt (Some ty) value
      in
      let loc = LocTest.gen () in
      top loc binding
    in
    let ty = Annot.Type.bool in
    let id = () |> Sym.seq |> Sym.gen in
    let annot =
      let patt = Annot.Ast.patt_var id in
      true
        |> Annot.Ast.bool
        |> Annot.Ast.binding patt ty
        |> Annot.Ast.top_let
    in
    Desug.desug_top env syntax (fun env top ->
      EnvTest.assert_bound ~ctxt AnnotTest.TypeTest.assert_ty_equal id env ty;
      AnnotTest.AstTest.assert_top_equal ~ctxt annot top))

let test_desug_top_let = assert_desug_top Syntax.Ast.top_let
let test_desug_top_val = assert_desug_top Syntax.Ast.top_val

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
              |> Syntax.Ast.patt_var loc
          in
          let ty =
            let loc = LocTest.gen () in
            Syntax.Type.constr loc bool_constr
          in
          let value =
            let loc = LocTest.gen () in
            Syntax.Ast.bool loc true
          in
          let binding =
            let loc = LocTest.gen () in
            Syntax.Ast.value_binding loc patt (Some ty) value
          in
          let loc = LocTest.gen () in
          Syntax.Ast.top_let loc binding
        in
        let top' =
          let patt =
            let loc = LocTest.gen () in
            seq
              |> Sym.gen
              |> Syntax.Ast.patt_var loc
          in
          let ty =
            let loc = LocTest.gen () in
            Syntax.Type.constr loc int_constr
          in
          let value =
            let loc = LocTest.gen () in
            Syntax.Ast.int loc "42"
          in
          let binding =
            let loc = LocTest.gen () in
            Syntax.Ast.value_binding loc patt (Some ty) value
          in
          let loc = LocTest.gen () in
          Syntax.Ast.top_val loc binding
        in
        let pkg =
          let name =
            let loc = LocTest.gen () in
            seq
              |> Sym.gen
              |> Syntax.Ast.name loc
          in
          let loc = LocTest.gen () in
          Syntax.Ast.pkg loc name
        in
        Syntax.Ast.file pkg [] [top; top']
      in
      let seq = Sym.seq () in
      let id = Sym.gen seq in
      let ty = Annot.Type.bool in
      let id' = Sym.gen seq in
      let ty' = Annot.Type.int in
      let annot =
        let top =
          let patt = Annot.Ast.patt_var id in
          true
            |> Annot.Ast.bool
            |> Annot.Ast.binding patt ty
            |> Annot.Ast.top_let
        in
        let top' =
          let patt = Annot.Ast.patt_var id' in
          42l
            |> Annot.Ast.int
            |> Annot.Ast.binding patt ty'
            |> Annot.Ast.top_let
        in
        [top; top']
      in
      Desug.desug_file env syntax (fun env tops ->
        List.iter (fun (id, ty) ->
          EnvTest.assert_bound ~ctxt AnnotTest.TypeTest.assert_ty_equal id env ty
        ) [(id, ty); (id', ty')];
        List.iter2 (AnnotTest.AstTest.assert_top_equal ~ctxt) annot tops)))

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
