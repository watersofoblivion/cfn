open OUnit2

open Common

open CommonTest

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
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Mono.ty_bool in
  let env = EnvTest.fresh () in
  let ir = Ir.atom_ident value in
  let mono = Mono.atom_ident value in
  Env.bind value bound env (fun env ->
    Monomorph.convert_atom env ir (fun ty atom ->
      MonoTest.assert_ty_equal ~ctxt bound ty;
      MonoTest.assert_atom_equal ~ctxt mono atom))

let test_convert_atom_ident_unbound _ =
  let value = () |> Sym.seq |> Sym.gen in
  let env = EnvTest.fresh () in
  let ir = Ir.atom_ident value in
  let exn = Monomorph.UnboundIdentifier value in
  assert_raises exn (fun _ ->
    Monomorph.convert_atom env ir (fun _ _ ->
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

(* Terms *)

let test_convert_term_let ctxt =
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
      |> Ir.term_expr
      |> Ir.term_let binding
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
      |> Mono.term_expr
      |> Mono.term_let binding
  in
  Monomorph.convert_term env ir (fun ty term ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_term_equal ~ctxt mono term)

let test_convert_term_expr ctxt =
  let env = EnvTest.fresh () in
  let ir = true |> Ir.atom_bool |> Ir.expr_atom |> Ir.term_expr in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom |> Mono.term_expr in
  Monomorph.convert_term env ir (fun ty term ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_term_equal ~ctxt mono term)

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
  "Abstract Syntax" >::: [
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
    "Binding" >:: test_convert_binding;
    "Terms" >::: [
      "Let Bindings" >:: test_convert_term_let;
      "Expressions"  >:: test_convert_term_expr;
    ];
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_convert_top_let;
    ];
  ]
