open OUnit2

open Common

open CommonTest

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
