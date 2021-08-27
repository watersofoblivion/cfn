open OUnit2

open Common

open CommonTest

let assert_convert_ty mono clos ctxt =
  let env = EnvTest.fresh () in
  ClosTest.TypeTest.assert_ty_equal ~ctxt clos
    |> Conv.convert_ty env mono

let test_convert_ty_bool = assert_convert_ty Mono.Type.bool Clos.Type.bool
let test_convert_ty_int = assert_convert_ty Mono.Type.int Clos.Type.int
let test_convert_ty_long = assert_convert_ty Mono.Type.long Clos.Type.long
let test_convert_ty_float = assert_convert_ty Mono.Type.float Clos.Type.float
let test_convert_ty_double = assert_convert_ty Mono.Type.double Clos.Type.double
let test_convert_ty_rune = assert_convert_ty Mono.Type.rune Clos.Type.rune
let test_convert_ty_string = assert_convert_ty Mono.Type.string Clos.Type.string

let test_convert_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.Ast.atom_bool true in
  let clos = Clos.Ast.atom_bool true in
  ClosTest.AstTest.assert_atom_equal ~ctxt clos
    |> Conv.convert_atom env mono

let test_convert_atom_int ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.Ast.atom_int 42l in
  let clos = Clos.Ast.atom_int 42l in
  ClosTest.AstTest.assert_atom_equal ~ctxt clos
    |> Conv.convert_atom env mono

let test_convert_atom_long ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.Ast.atom_long 42L in
  let clos = Clos.Ast.atom_long 42L in
  ClosTest.AstTest.assert_atom_equal ~ctxt clos
    |> Conv.convert_atom env mono

let test_convert_atom_float ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.Ast.atom_float 4.2 in
  let clos = Clos.Ast.atom_float 4.2 in
  ClosTest.AstTest.assert_atom_equal ~ctxt clos
    |> Conv.convert_atom env mono

let test_convert_atom_double ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.Ast.atom_double 4.2 in
  let clos = Clos.Ast.atom_double 4.2 in
  ClosTest.AstTest.assert_atom_equal ~ctxt clos
    |> Conv.convert_atom env mono

let test_convert_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let mono = Mono.Ast.atom_rune value in
  let clos = Clos.Ast.atom_rune value in
  ClosTest.AstTest.assert_atom_equal ~ctxt clos
    |> Conv.convert_atom env mono

let test_convert_atom_string ctxt =
  let env = EnvTest.fresh () in
  let value =
    "foo bar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
  in
  let mono = Mono.Ast.atom_string value in
  let clos = Clos.Ast.atom_string value in
  ClosTest.AstTest.assert_atom_equal ~ctxt clos
    |> Conv.convert_atom env mono

let test_convert_atom_ident ctxt =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let mono = Mono.Ast.atom_ident value in
  let clos = Clos.Ast.atom_ident value in
  ClosTest.AstTest.assert_atom_equal ~ctxt clos
    |> Conv.convert_atom env mono

let test_convert_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Mono.Ast.atom_bool |> Mono.Ast.expr_atom in
  let clos = true |> Clos.Ast.atom_bool |> Clos.Ast.expr_atom in
  ClosTest.AstTest.assert_expr_equal ~ctxt clos
    |> Conv.convert_expr env mono

let test_convert_block_expr ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Mono.Ast.atom_bool |> Mono.Ast.expr_atom |> Mono.Ast.block_expr in
  let clos = true |> Clos.Ast.atom_bool |> Clos.Ast.expr_atom |> Clos.Ast.block_expr in
  ClosTest.AstTest.assert_block_equal ~ctxt clos
    |> Conv.convert_block env mono

let test_convert_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.Ast.patt_ground in
  let clos = Clos.Ast.patt_ground in
  ClosTest.AstTest.assert_patt_equal ~ctxt clos
    |> Conv.convert_patt env mono

let test_convert_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let mono = Mono.Ast.patt_var id in
  let clos = Clos.Ast.patt_var id in
  ClosTest.AstTest.assert_patt_equal ~ctxt clos
    |> Conv.convert_patt env mono

let test_convert_binding ctxt =
  let env = EnvTest.fresh () in
  let mono =
    let patt = Mono.Ast.patt_ground in
    let ty = Mono.Type.bool in
    true
      |> Mono.Ast.atom_bool
      |> Mono.Ast.expr_atom
      |> Mono.Ast.binding patt ty
  in
  let clos =
    let patt = Clos.Ast.patt_ground in
    let ty = Clos.Type.bool in
    true
      |> Clos.Ast.atom_bool
      |> Clos.Ast.expr_atom
      |> Clos.Ast.binding patt ty
  in
  ClosTest.AstTest.assert_binding_equal ~ctxt clos
    |> Conv.convert_binding env mono

let test_convert_top_let ctxt =
  let env = EnvTest.fresh () in
  let mono =
    let patt = Mono.Ast.patt_ground in
    let ty = Mono.Type.bool in
    true
      |> Mono.Ast.atom_bool
      |> Mono.Ast.expr_atom
      |> Mono.Ast.binding patt ty
      |> Mono.Ast.top_let
  in
  let clos =
    let patt = Clos.Ast.patt_ground in
    let ty = Clos.Type.bool in
    true
      |> Clos.Ast.atom_bool
      |> Clos.Ast.expr_atom
      |> Clos.Ast.binding patt ty
      |> Clos.Ast.top_let
  in
  ClosTest.AstTest.assert_top_equal ~ctxt clos
    |> Conv.convert_top env mono

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
