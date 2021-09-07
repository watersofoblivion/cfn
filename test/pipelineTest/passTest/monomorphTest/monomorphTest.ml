open OUnit2

open Common

open CommonTest

(* Types *)

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

(* Builtins *)

let test_convert_builtin_add ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_add Ir.ty_int in
  let mono = Mono.builtin_add Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_sub ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_sub Ir.ty_int in
  let mono = Mono.builtin_sub Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_mul ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_mul Ir.ty_int in
  let mono = Mono.builtin_mul Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_div ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_div Ir.ty_int in
  let mono = Mono.builtin_div Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_mod ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_mod Ir.ty_int in
  let mono = Mono.builtin_mod Mono.ty_int in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_exp ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_exp Ir.ty_float in
  let mono = Mono.builtin_exp Mono.ty_float in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_promote ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_promote Ir.ty_int Ir.ty_long in
  let mono = Mono.builtin_promote Mono.ty_int Mono.ty_long in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

let test_convert_builtin_concat ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.builtin_concat Ir.ty_string in
  let mono = Mono.builtin_concat Mono.ty_string in
  Monomorph.convert_builtin env ir (fun builtin ->
    MonoTest.assert_builtin_equal ~ctxt mono builtin)

(* Atomic Values *)

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
  let value = "foo bar" in
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

(* Expressions *)

let test_convert_expr_builtin ctxt =
  let env = EnvTest.fresh () in
  let values = [1l; 2l] in
  let ir =
    let builtin = Ir.builtin_add Ir.ty_int in
    values
      |> List.map Ir.atom_int
      |> Ir.expr_builtin builtin
  in
  let mono =
    let builtin = Mono.builtin_add Mono.ty_int in
    values
      |> List.map Mono.atom_int
      |> Mono.expr_builtin builtin
  in
  Monomorph.convert_expr env ir (fun ty expr ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_int ty;
    MonoTest.assert_expr_equal ~ctxt mono expr)

let test_convert_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let ir = true |> Ir.atom_bool |> Ir.expr_atom in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom in
  Monomorph.convert_expr env ir (fun ty expr ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_expr_equal ~ctxt mono expr)

(* Patterns *)

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

(* Bindings *)

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

(* Blocks *)

let test_convert_block_let ctxt =
  let env = EnvTest.fresh () in
  let sym = () |> Sym.seq |> Sym.gen in
  let ir =
    let binding =
      let patt = Ir.patt_var sym in
      let ty = Ir.ty_bool in
      true
        |> Ir.atom_bool
        |> Ir.expr_atom
        |> Ir.binding patt ty
    in
    sym
      |> Ir.atom_ident
      |> Ir.expr_atom
      |> Ir.block_expr
      |> Ir.block_let binding
  in
  let mono =
    let binding =
      let patt = Mono.patt_var sym in
      let ty = Mono.ty_bool in
      true
        |> Mono.atom_bool
        |> Mono.expr_atom
        |> Mono.binding patt ty
    in
    sym
      |> Mono.atom_ident
      |> Mono.expr_atom
      |> Mono.block_expr
      |> Mono.block_let binding
  in
  Monomorph.convert_block env ir (fun ty block ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_block_equal ~ctxt mono block)

let test_convert_block_expr ctxt =
  let env = EnvTest.fresh () in
  let ir = true |> Ir.atom_bool |> Ir.expr_atom |> Ir.block_expr in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom |> Mono.block_expr in
  Monomorph.convert_block env ir (fun ty block ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_block_equal ~ctxt mono block)

(* Top-Level Expressions *)

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

(* Test Suite *)

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
    "Built-in Functions" >::: [
      "Addition"       >:: test_convert_builtin_add;
      "Subtraction"    >:: test_convert_builtin_sub;
      "Multiplication" >:: test_convert_builtin_mul;
      "Division"       >:: test_convert_builtin_div;
      "Modulus"        >:: test_convert_builtin_mod;
      "Exponentiation" >:: test_convert_builtin_exp;
      "Type Promotion" >:: test_convert_builtin_promote;
      "Concatenation"  >:: test_convert_builtin_concat;
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
      "Built-in Function Application" >:: test_convert_expr_builtin;
      "Atomic Values"                 >:: test_convert_expr_atom;
    ];
    "Patterns" >::: [
      "Ground"     >:: test_convert_patt_ground;
      "Identifier" >:: test_convert_patt_var;
    ];
    "Binding" >:: test_convert_binding;
    "Blocks" >::: [
      "Let Bindings" >:: test_convert_block_let;
      "Expressions"  >:: test_convert_block_expr;
    ];
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_convert_top_let;
    ];
  ]
