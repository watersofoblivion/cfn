(* Terms *)

open OUnit2

open Common

open CommonTest

(* Tests *)

(* Bindings *)

let test_convert_binding ctxt =
  let env = EnvTest.fresh () in
  let mono =
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
  let clos =
    let patt = Mono.patt_var id in
    true
      |> Mono.atom_bool
      |> Mono.expr_atom
      |> Mono.binding patt ty
  in
  Monomorph.convert_binding env mono (fun env binding ->
    EnvTest.assert_bound ~ctxt MonoTest.assert_ty_equal id env ty;
    MonoTest.assert_binding_equal ~ctxt clos binding)

let test_convert_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let mono =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Ir.patt_var
    in
    let ty = Ir.ty_bool in
    42l
      |> Ir.atom_int
      |> Ir.expr_atom
      |> Ir.binding patt ty
  in
  let exn = Monomorph.MismatchedTypes (Mono.ty_int, Mono.ty_bool) in
  assert_raises exn (fun _ ->
    Monomorph.convert_binding env mono (fun _ _ ->
      assert_failure "Expected exception"))

(* Terms *)

let test_convert_term_let ctxt =
  let env = EnvTest.fresh () in
  let sym = () |> Sym.seq |> Sym.gen in
  let mono =
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
  let clos =
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
  Monomorph.convert_term env mono (fun ty term ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_term_equal ~ctxt clos term)

let test_convert_term_expr ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Ir.atom_bool |> Ir.expr_atom |> Ir.term_expr in
  let clos = true |> Mono.atom_bool |> Mono.expr_atom |> Mono.term_expr in
  Monomorph.convert_term env mono (fun ty term ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_term_equal ~ctxt clos term)

(* Test Suite *)

let suite =
  "Terms" >::: [
    "Binding" >::: [
      "Valid"            >:: test_convert_binding;
      "Mismatched Types" >:: test_convert_binding_mismatched_types;
    ];
    "Terms" >::: [
      "Let Bindings" >:: test_convert_term_let;
      "Expressions"  >:: test_convert_term_expr;
    ];
  ]
