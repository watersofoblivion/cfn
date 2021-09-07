open OUnit2

open Common

open CommonTest

(* Types *)

let assert_convert_ty mono clos ctxt =
  let env = EnvTest.fresh () in
  ClosTest.assert_ty_equal ~ctxt clos
    |> Conv.convert_ty env mono

let test_convert_ty_bool = assert_convert_ty Mono.ty_bool Clos.ty_bool
let test_convert_ty_int = assert_convert_ty Mono.ty_int Clos.ty_int
let test_convert_ty_long = assert_convert_ty Mono.ty_long Clos.ty_long
let test_convert_ty_float = assert_convert_ty Mono.ty_float Clos.ty_float
let test_convert_ty_double = assert_convert_ty Mono.ty_double Clos.ty_double
let test_convert_ty_rune = assert_convert_ty Mono.ty_rune Clos.ty_rune
let test_convert_ty_string = assert_convert_ty Mono.ty_string Clos.ty_string

(* Builtins *)

let test_convert_builtin_add ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_add Mono.ty_int in
  let clos = Clos.builtin_add Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_sub ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_sub Mono.ty_int in
  let clos = Clos.builtin_sub Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_mul ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_mul Mono.ty_int in
  let clos = Clos.builtin_mul Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_div ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_div Mono.ty_int in
  let clos = Clos.builtin_div Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_mod ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_mod Mono.ty_int in
  let clos = Clos.builtin_mod Clos.ty_int in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_exp ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_exp Mono.ty_float in
  let clos = Clos.builtin_exp Clos.ty_float in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_promote ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_promote Mono.ty_int Mono.ty_long in
  let clos = Clos.builtin_promote Clos.ty_int Clos.ty_long in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

let test_convert_builtin_concat ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.builtin_concat Mono.ty_string in
  let clos = Clos.builtin_concat Clos.ty_string in
  Conv.convert_builtin env mono (fun builtin ->
    ClosTest.assert_builtin_equal ~ctxt clos builtin)

(* Atomic Values *)

let test_convert_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_bool true in
  let clos = Clos.atom_bool true in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_bool ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_int ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_int 42l in
  let clos = Clos.atom_int 42l in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_int ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_long ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_long 42L in
  let clos = Clos.atom_long 42L in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_long ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_float ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_float 4.2 in
  let clos = Clos.atom_float 4.2 in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_float ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_double ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_double 4.2 in
  let clos = Clos.atom_double 4.2 in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_double ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let mono = Mono.atom_rune value in
  let clos = Clos.atom_rune value in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_rune ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_string ctxt =
  let env = EnvTest.fresh () in
  let value = "foo bar" in
  let mono = Mono.atom_string value in
  let clos = Clos.atom_string value in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_string ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_ident ctxt =
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Clos.ty_bool in
  let env = EnvTest.fresh () in
  let mono = Mono.atom_ident value in
  let clos = Clos.atom_ident value in
  Env.bind value bound env (fun env ->
    Conv.convert_atom env mono (fun ty atom ->
      ClosTest.assert_ty_equal ~ctxt bound ty;
      ClosTest.assert_atom_equal ~ctxt clos atom))

let test_convert_atom_ident_unbound _ =
  let value = () |> Sym.seq |> Sym.gen in
  let env = EnvTest.fresh () in
  let mono = Mono.atom_ident value in
  let exn = Conv.UnboundIdentifier value in
  assert_raises exn (fun _ ->
    Conv.convert_atom env mono (fun _ _ ->
      assert_failure "Expected exception"))

(* Expressions *)

let test_convert_expr_builtin ctxt =
  let env = EnvTest.fresh () in
  let values = [1l; 2l] in
  let mono =
    let builtin = Mono.builtin_add Mono.ty_int in
    values
      |> List.map Mono.atom_int
      |> Mono.expr_builtin builtin
  in
  let clos =
    let builtin = Clos.builtin_add Clos.ty_int in
    values
      |> List.map Clos.atom_int
      |> Clos.expr_builtin builtin
  in
  Conv.convert_expr env mono (fun ty expr ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_int ty;
    ClosTest.assert_expr_equal ~ctxt clos expr)

let test_convert_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom in
  let clos = true |> Clos.atom_bool |> Clos.expr_atom in
  Conv.convert_expr env mono (fun ty expr ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_bool ty;
    ClosTest.assert_expr_equal ~ctxt clos expr)

(* Patterns *)

let test_convert_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.patt_ground in
  let clos = Clos.patt_ground in
  Conv.convert_patt env mono Clos.ty_bool (fun _ patt ->
    ClosTest.assert_patt_equal ~ctxt clos patt)

let test_convert_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Clos.ty_bool in
  let mono = Mono.patt_var id in
  let clos = Clos.patt_var id in
  Conv.convert_patt env mono ty (fun env patt ->
    EnvTest.assert_bound ~ctxt ClosTest.assert_ty_equal id env ty;
    ClosTest.assert_patt_equal ~ctxt clos patt)

(* Bindings *)

let test_convert_binding ctxt =
  let env = EnvTest.fresh () in
  let mono =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Mono.patt_var
    in
    let ty = Mono.ty_bool in
    true
      |> Mono.atom_bool
      |> Mono.expr_atom
      |> Mono.binding patt ty
  in
  let id =
    ()
      |> Sym.seq
      |> Sym.gen
  in
  let ty = Clos.ty_bool in
  let clos =
    let patt = Clos.patt_var id in
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
      |> Clos.binding patt ty
  in
  Conv.convert_binding env mono (fun env binding ->
    EnvTest.assert_bound ~ctxt ClosTest.assert_ty_equal id env ty;
    ClosTest.assert_binding_equal ~ctxt clos binding)

(* Blocks *)

let test_convert_block_let ctxt =
  let env = EnvTest.fresh () in
  let sym = () |> Sym.seq |> Sym.gen in
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
  let clos =
    let binding =
      let patt = Clos.patt_var sym in
      let ty = Clos.ty_bool in
      true
        |> Clos.atom_bool
        |> Clos.expr_atom
        |> Clos.binding patt ty
    in
    sym
      |> Clos.atom_ident
      |> Clos.expr_atom
      |> Clos.block_expr
      |> Clos.block_let binding
  in
  Conv.convert_block env mono (fun ty block ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_bool ty;
    ClosTest.assert_block_equal ~ctxt clos block)

let test_convert_block_expr ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom |> Mono.block_expr in
  let clos = true |> Clos.atom_bool |> Clos.expr_atom |> Clos.block_expr in
  Conv.convert_block env mono (fun ty block ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_bool ty;
    ClosTest.assert_block_equal ~ctxt clos block)

(* Top-Level Expressions *)

let test_convert_top_let ctxt =
  let env = EnvTest.fresh () in
  let mono =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Mono.patt_var
    in
    let ty = Mono.ty_bool in
    true
      |> Mono.atom_bool
      |> Mono.expr_atom
      |> Mono.binding patt ty
      |> Mono.top_let
  in
  let id =
    ()
      |> Sym.seq
      |> Sym.gen
  in
  let ty = Clos.ty_bool in
  let clos =
    let patt = Clos.patt_var id in
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
      |> Clos.binding patt ty
      |> Clos.top_let
  in
  Conv.convert_top env mono (fun env top ->
    EnvTest.assert_bound ~ctxt ClosTest.assert_ty_equal id env ty;
    ClosTest.assert_top_equal ~ctxt clos top)

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
