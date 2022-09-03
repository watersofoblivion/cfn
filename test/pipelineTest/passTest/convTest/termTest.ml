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

let test_convert_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let mono =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Mono.patt_var
    in
    let ty = Mono.ty_bool in
    42l
      |> Mono.atom_int
      |> Mono.expr_atom
      |> Mono.binding patt ty
  in
  let exn = Conv.MismatchedTypes (Clos.ty_int, Clos.ty_bool) in
  assert_raises exn (fun _ ->
    Conv.convert_binding env mono (fun _ _ ->
      assert_failure "Expected exception"))

(* Terms *)

let test_convert_term_let ctxt =
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
      |> Mono.term_expr
      |> Mono.term_let binding
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
      |> Clos.term_expr
      |> Clos.term_let binding
  in
  Conv.convert_term env mono (fun ty term ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_bool ty;
    ClosTest.assert_term_equal ~ctxt clos term)

let test_convert_term_expr ctxt =
  let env = EnvTest.fresh () in
  let mono = true |> Mono.atom_bool |> Mono.expr_atom |> Mono.term_expr in
  let clos = true |> Clos.atom_bool |> Clos.expr_atom |> Clos.term_expr in
  Conv.convert_term env mono (fun ty term ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_bool ty;
    ClosTest.assert_term_equal ~ctxt clos term)

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
