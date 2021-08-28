open OUnit2

open Common

open CommonTest

let assert_convert_ty ir mono ctxt =
  let env = EnvTest.fresh () in
  MonoTest.assert_ty_equal ~ctxt mono
    |> Monomorph.convert_ty env ir

let test_convert_ty_bool = assert_convert_ty Ir.ty_bool Mono.ty_bool
let test_convert_ty_int = assert_convert_ty Ir.ty_int Mono.ty_int
let test_convert_ty_long = assert_convert_ty Ir.ty_long Mono.ty_long
let test_convert_ty_float = assert_convert_ty Ir.ty_float Mono.ty_float
let test_convert_ty_double = assert_convert_ty Ir.ty_double Mono.ty_double
let test_convert_ty_rune = assert_convert_ty Ir.ty_rune Mono.ty_rune
let test_convert_ty_string = assert_convert_ty Ir.ty_string Mono.ty_string

let test_convert_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_bool true in
  let mono = Mono.atom_bool true in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_int ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_int 42l in
  let mono = Mono.atom_int 42l in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_int ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_long ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_long 42L in
  let mono = Mono.atom_long 42L in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_long ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_float ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_float 4.2 in
  let mono = Mono.atom_float 4.2 in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_float ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_double ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_double 4.2 in
  let mono = Mono.atom_double 4.2 in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_double ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let ir = Ir.atom_rune value in
  let mono = Mono.atom_rune value in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_rune ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_string ctxt =
  let env = EnvTest.fresh () in
  let value =
    "foo bar"
      |> String.to_seq
      |> List.of_seq
      |> List.map Uchar.of_char
  in
  let ir = Ir.atom_string value in
  let mono = Mono.atom_string value in
  Monomorph.convert_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_string ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_convert_atom_ident ctxt =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Mono.ty_bool in
  let ir = Ir.atom_ident value in
  let mono = Mono.atom_ident value in
  Env.bind value bound env (fun env ->
    Monomorph.convert_atom env ir (fun ty atom ->
      MonoTest.assert_ty_equal ~ctxt bound ty;
      MonoTest.assert_atom_equal ~ctxt mono atom))

let test_convert_atom_ident_unbound _ =
  let env = EnvTest.fresh () in
  let value = () |> Sym.seq |> Sym.gen in
  let ir = Ir.atom_ident value in
  let exn = Monomorph.UnboundIdentifier value in
  assert_raises exn (fun _ ->
    Monomorph.convert_atom env ir (fun _ ->
      assert_failure "Expected exception"))

let test_convert_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let ir = true |> Ir.atom_bool |> Ir.expr_atom in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom in
  Monomorph.convert_expr env ir (fun ty expr ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_expr_equal ~ctxt mono expr)

let test_convert_block_expr ctxt =
  let env = EnvTest.fresh () in
  let ir = true |> Ir.atom_bool |> Ir.expr_atom |> Ir.block_expr in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom |> Mono.block_expr in
  Monomorph.convert_block env ir (fun ty block ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_block_equal ~ctxt mono block)

let test_convert_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.patt_ground in
  let mono = Mono.patt_ground in
  Monomorph.convert_patt env ir Mono.ty_bool (fun _ patt ->
    MonoTest.assert_patt_equal ~ctxt mono patt)

let test_convert_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Mono.ty_bool in
  let ir = Ir.patt_var id in
  let mono = Mono.patt_var id in
  Monomorph.convert_patt env ir Mono.ty_bool (fun env patt ->
    EnvTest.assert_bound ~ctxt MonoTest.assert_ty_equal id env ty;
    MonoTest.assert_patt_equal ~ctxt mono patt)

let test_convert_binding ctxt =
  let env = EnvTest.fresh () in
  let ir =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Ir.patt_var
    in
    let ty = Ir.ty_bool in
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.binding patt ty
  in
  let id =
    ()
      |> Sym.seq
      |> Sym.gen
  in
  let ty = Mono.ty_bool in
  let mono =
    let patt = Mono.patt_var id in
    true
      |> Mono.atom_bool
      |> Mono.expr_atom
      |> Mono.binding patt ty
  in
  Monomorph.convert_binding env ir (fun env binding ->
    EnvTest.assert_bound ~ctxt MonoTest.assert_ty_equal id env ty;
    MonoTest.assert_binding_equal ~ctxt mono binding)

let test_convert_top_let ctxt =
  let env = EnvTest.fresh () in
  let ir =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Ir.patt_var
    in
    let ty = Ir.ty_bool in
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.binding patt ty
      |> Ir.top_let
  in
  let id =
    ()
      |> Sym.seq
      |> Sym.gen
  in
  let ty = Mono.ty_bool in
  let mono =
    let patt = Mono.patt_var id in
    true
      |> Mono.atom_bool
      |> Mono.expr_atom
      |> Mono.binding patt ty
      |> Mono.top_let
  in
  Monomorph.convert_top env ir (fun env top ->
    EnvTest.assert_bound ~ctxt MonoTest.assert_ty_equal id env ty;
    MonoTest.assert_top_equal ~ctxt mono top)

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
