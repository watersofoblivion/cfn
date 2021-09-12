(* Terms *)

open OUnit2

open Common

open CommonTest

(* Tests *)

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

(* Test Suite *)

let suite =
  "Terms" >::: [
    "Binding" >:: test_convert_binding;
    "Terms" >::: [
      "Let Bindings" >:: test_convert_term_let;
      "Expressions"  >:: test_convert_term_expr;
    ];
  ]
