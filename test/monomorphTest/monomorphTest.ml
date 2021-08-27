open OUnit2

open Common

open CommonTest

let assert_convert_ty mono clos ctxt =
  let env = EnvTest.fresh () in
  MonoTest.TypeTest.assert_ty_equal ~ctxt clos
    |> Monomorph.convert_ty env mono

let test_convert_ty_bool = assert_convert_ty Ir.Type.bool Mono.Type.bool
let test_convert_ty_int = assert_convert_ty Ir.Type.int Mono.Type.int
let test_convert_ty_long = assert_convert_ty Ir.Type.long Mono.Type.long
let test_convert_ty_float = assert_convert_ty Ir.Type.float Mono.Type.float
let test_convert_ty_double = assert_convert_ty Ir.Type.double Mono.Type.double
let test_convert_ty_rune = assert_convert_ty Ir.Type.rune Mono.Type.rune
let test_convert_ty_string = assert_convert_ty Ir.Type.string Mono.Type.string

let test_convert_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let mono = Ir.Ast.atom_bool true in
  let clos = Mono.Ast.atom_bool true in
  MonoTest.AstTest.assert_atom_equal ~ctxt clos
    |> Monomorph.convert_atom env mono

let test_convert_atom_int ctxt =
  let env = EnvTest.fresh () in
  let mono = Ir.Ast.atom_int 42l in
  let clos = Mono.Ast.atom_int 42l in
  MonoTest.AstTest.assert_atom_equal ~ctxt clos
    |> Monomorph.convert_atom env mono

let test_convert_atom_long ctxt =
  let env = EnvTest.fresh () in
  let mono = Ir.Ast.atom_long 42L in
  let clos = Mono.Ast.atom_long 42L in
  MonoTest.AstTest.assert_atom_equal ~ctxt clos
    |> Monomorph.convert_atom env mono

let test_convert_atom_float ctxt =
  let env = EnvTest.fresh () in
  let mono = Ir.Ast.atom_float 4.2 in
  let clos = Mono.Ast.atom_float 4.2 in
  MonoTest.AstTest.assert_atom_equal ~ctxt clos
    |> Monomorph.convert_atom env mono

let test_convert_atom_double ctxt =
  let env = EnvTest.fresh () in
  let mono = Ir.Ast.atom_double 4.2 in
  let clos = Mono.Ast.atom_double 4.2 in
  MonoTest.AstTest.assert_atom_equal ~ctxt clos
    |> Monomorph.convert_atom env mono

let test_convert_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let mono = Ir.Ast.atom_rune value in
  let clos = Mono.Ast.atom_rune value in
  MonoTest.AstTest.assert_atom_equal ~ctxt clos
    |> Monomorph.convert_atom env mono

let test_convert_atom_string ctxt =
  let env = EnvTest.fresh () in
  let value =
    "foo bar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
  in
  let mono = Ir.Ast.atom_string value in
  let clos = Mono.Ast.atom_string value in
  MonoTest.AstTest.assert_atom_equal ~ctxt clos
    |> Monomorph.convert_atom env mono

let test_convert_atom_ident ctxt =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let mono = Ir.Ast.atom_ident value in
  let clos = Mono.Ast.atom_ident value in
  MonoTest.AstTest.assert_atom_equal ~ctxt clos
    |> Monomorph.convert_atom env mono

let test_convert_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Ir.Ast.atom_bool |> Ir.Ast.expr_atom in
  let clos = true |> Mono.Ast.atom_bool |> Mono.Ast.expr_atom in
  MonoTest.AstTest.assert_expr_equal ~ctxt clos
    |> Monomorph.convert_expr env mono

let test_convert_block_expr ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Ir.Ast.atom_bool |> Ir.Ast.expr_atom |> Ir.Ast.block_expr in
  let clos = true |> Mono.Ast.atom_bool |> Mono.Ast.expr_atom |> Mono.Ast.block_expr in
  MonoTest.AstTest.assert_block_equal ~ctxt clos
    |> Monomorph.convert_block env mono

let test_convert_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let mono = Ir.Ast.patt_ground in
  let clos = Mono.Ast.patt_ground in
  MonoTest.AstTest.assert_patt_equal ~ctxt clos
    |> Monomorph.convert_patt env mono

let test_convert_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let mono = Ir.Ast.patt_var id in
  let clos = Mono.Ast.patt_var id in
  MonoTest.AstTest.assert_patt_equal ~ctxt clos
    |> Monomorph.convert_patt env mono

let test_convert_binding ctxt =
  let env = EnvTest.fresh () in
  let mono =
    let patt = Ir.Ast.patt_ground in
    let ty = Ir.Type.bool in
    true
      |> Ir.Ast.atom_bool
      |> Ir.Ast.expr_atom
      |> Ir.Ast.binding patt ty
  in
  let clos =
    let patt = Mono.Ast.patt_ground in
    let ty = Mono.Type.bool in
    true
      |> Mono.Ast.atom_bool
      |> Mono.Ast.expr_atom
      |> Mono.Ast.binding patt ty
  in
  MonoTest.AstTest.assert_binding_equal ~ctxt clos
    |> Monomorph.convert_binding env mono

let test_convert_top_let ctxt =
  let env = EnvTest.fresh () in
  let mono =
    let patt = Ir.Ast.patt_ground in
    let ty = Ir.Type.bool in
    true
      |> Ir.Ast.atom_bool
      |> Ir.Ast.expr_atom
      |> Ir.Ast.binding patt ty
      |> Ir.Ast.top_let
  in
  let clos =
    let patt = Mono.Ast.patt_ground in
    let ty = Mono.Type.bool in
    true
      |> Mono.Ast.atom_bool
      |> Mono.Ast.expr_atom
      |> Mono.Ast.binding patt ty
      |> Mono.Ast.top_let
  in
  MonoTest.AstTest.assert_top_equal ~ctxt clos
    |> Monomorph.convert_top env mono

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
      "Identifiers" >:: test_convert_atom_ident;
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
