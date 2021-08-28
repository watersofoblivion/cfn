open OUnit2

open Common

open CommonTest

let assert_convert_ty ir mono ctxt =
  let env = EnvTest.fresh () in
  MonoTest.TypeTest.assert_ty_equal ~ctxt mono
    |> Monomorph.convert_ty env ir

let test_convert_ty_bool = assert_convert_ty Ir.Type.bool Mono.Type.bool
let test_convert_ty_int = assert_convert_ty Ir.Type.int Mono.Type.int
let test_convert_ty_long = assert_convert_ty Ir.Type.long Mono.Type.long
let test_convert_ty_float = assert_convert_ty Ir.Type.float Mono.Type.float
let test_convert_ty_double = assert_convert_ty Ir.Type.double Mono.Type.double
let test_convert_ty_rune = assert_convert_ty Ir.Type.rune Mono.Type.rune
let test_convert_ty_string = assert_convert_ty Ir.Type.string Mono.Type.string

let test_convert_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.Ast.atom_bool true in
  let mono = Mono.Ast.atom_bool true in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.TypeTest.assert_ty_equal ~ctxt Mono.Type.bool ty;
    MonoTest.AstTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_int ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.Ast.atom_int 42l in
  let mono = Mono.Ast.atom_int 42l in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.TypeTest.assert_ty_equal ~ctxt Mono.Type.int ty;
    MonoTest.AstTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_long ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.Ast.atom_long 42L in
  let mono = Mono.Ast.atom_long 42L in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.TypeTest.assert_ty_equal ~ctxt Mono.Type.long ty;
    MonoTest.AstTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_float ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.Ast.atom_float 4.2 in
  let mono = Mono.Ast.atom_float 4.2 in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.TypeTest.assert_ty_equal ~ctxt Mono.Type.float ty;
    MonoTest.AstTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_double ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.Ast.atom_double 4.2 in
  let mono = Mono.Ast.atom_double 4.2 in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.TypeTest.assert_ty_equal ~ctxt Mono.Type.double ty;
    MonoTest.AstTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let ir = Ir.Ast.atom_rune value in
  let mono = Mono.Ast.atom_rune value in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.TypeTest.assert_ty_equal ~ctxt Mono.Type.rune ty;
    MonoTest.AstTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_string ctxt =
  let env = EnvTest.fresh () in
  let value =
    "foo bar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
  in
  let ir = Ir.Ast.atom_string value in
  let mono = Mono.Ast.atom_string value in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.TypeTest.assert_ty_equal ~ctxt Mono.Type.string ty;
    MonoTest.AstTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_ident ctxt =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Mono.Type.bool in
  let ir = Ir.Ast.atom_ident value in
  let mono = Mono.Ast.atom_ident value in
  Env.bind value bound env (fun env ->
    Monomorph.convert_atom env ir (fun ty atom ->
      MonoTest.TypeTest.assert_ty_equal ~ctxt bound ty;
      MonoTest.AstTest.assert_atom_equal ~ctxt mono atom))

let test_convert_atom_ident_unbound _ =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let ir = Ir.Ast.atom_ident value in
  let exn = Monomorph.UnboundIdentifier value in
  assert_raises exn (fun _ ->
    Monomorph.convert_atom env ir (fun _ ->
      assert_failure "Expected exception"))

let test_convert_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let ir = true |> Ir.Ast.atom_bool |> Ir.Ast.expr_atom in
  let mono = true |> Mono.Ast.atom_bool |> Mono.Ast.expr_atom in
  Monomorph.convert_expr env ir (fun ty expr ->
    MonoTest.TypeTest.assert_ty_equal ~ctxt Mono.Type.bool ty;
    MonoTest.AstTest.assert_expr_equal ~ctxt mono expr)

let test_convert_block_expr ctxt =
  let env = EnvTest.fresh () in
  let ir = true |> Ir.Ast.atom_bool |> Ir.Ast.expr_atom |> Ir.Ast.block_expr in
  let mono = true |> Mono.Ast.atom_bool |> Mono.Ast.expr_atom |> Mono.Ast.block_expr in
  Monomorph.convert_block env ir (fun ty block ->
    MonoTest.TypeTest.assert_ty_equal ~ctxt Mono.Type.bool ty;
    MonoTest.AstTest.assert_block_equal ~ctxt mono block)

let test_convert_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.Ast.patt_ground in
  let mono = Mono.Ast.patt_ground in
  Monomorph.convert_patt env ir Mono.Type.bool (fun _ patt ->
    MonoTest.AstTest.assert_patt_equal ~ctxt mono patt)

let test_convert_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Mono.Type.bool in
  let ir = Ir.Ast.patt_var id in
  let mono = Mono.Ast.patt_var id in
  Monomorph.convert_patt env ir Mono.Type.bool (fun env patt ->
    EnvTest.assert_bound ~ctxt MonoTest.TypeTest.assert_ty_equal id env ty;
    MonoTest.AstTest.assert_patt_equal ~ctxt mono patt)

let test_convert_binding ctxt =
  let env = EnvTest.fresh () in
  let ir =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Ir.Ast.patt_var
    in
    let ty = Ir.Type.bool in
    true
      |> Ir.Ast.atom_bool
      |> Ir.Ast.expr_atom
      |> Ir.Ast.binding patt ty
  in
  let id =
    ()
      |> Sym.seq
      |> Sym.gen
  in
  let ty = Mono.Type.bool in
  let mono =
    let patt = Mono.Ast.patt_var id in
    true
      |> Mono.Ast.atom_bool
      |> Mono.Ast.expr_atom
      |> Mono.Ast.binding patt ty
  in
  Monomorph.convert_binding env ir (fun env binding ->
    EnvTest.assert_bound ~ctxt MonoTest.TypeTest.assert_ty_equal id env ty;
    MonoTest.AstTest.assert_binding_equal ~ctxt mono binding)

let test_convert_top_let ctxt =
  let env = EnvTest.fresh () in
  let ir =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Ir.Ast.patt_var
    in
    let ty = Ir.Type.bool in
    true
      |> Ir.Ast.atom_bool
      |> Ir.Ast.expr_atom
      |> Ir.Ast.binding patt ty
      |> Ir.Ast.top_let
  in
  let id =
    ()
      |> Sym.seq
      |> Sym.gen
  in
  let ty = Mono.Type.bool in
  let mono =
    let patt = Mono.Ast.patt_var id in
    true
      |> Mono.Ast.atom_bool
      |> Mono.Ast.expr_atom
      |> Mono.Ast.binding patt ty
      |> Mono.Ast.top_let
  in
  Monomorph.convert_top env ir (fun env top ->
    EnvTest.assert_bound ~ctxt MonoTest.TypeTest.assert_ty_equal id env ty;
    MonoTest.AstTest.assert_top_equal ~ctxt mono top)

let suite =
  "Closure Conversion" >::: [
    "Types" >::: [
      "Boolean" >:: test_convert_ty_bool;
      "Integer" >:: test_convert_ty_int;
      "Long"    >:: test_convert_ty_long;
      "Float"   >:: test_convert_ty_float;
      "Double"  >:: test_convert_ty_double;
      "Rune"    >:: test_convert_ty_rune;
      "String"  >:: test_convert_ty_string;
    ];
    "Atomic Values" >::: [
      "Booleans"    >:: test_convert_atom_bool;
      "Integers"    >:: test_convert_atom_int;
      "Longs"       >:: test_convert_atom_long;
      "Floats"      >:: test_convert_atom_float;
      "Doubles"     >:: test_convert_atom_double;
      "Runes"       >:: test_convert_atom_rune;
      "Strings"     >:: test_convert_atom_string;
      "Identifiers" >::: [
        "Bound"   >:: test_convert_atom_ident;
        "Unbound" >:: test_convert_atom_ident_unbound;
      ];
    ];
    "Expressions" >::: [
      "Atomic Values" >:: test_convert_expr_atom;
    ];
    "Blocks" >::: [
      "Expressions" >:: test_convert_block_expr;
    ];
    "Patterns" >::: [
      "Ground"     >:: test_convert_patt_ground;
      "Identifier" >:: test_convert_patt_var;
    ];
    "Binding" >:: test_convert_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_convert_top_let;
    ];
  ]
