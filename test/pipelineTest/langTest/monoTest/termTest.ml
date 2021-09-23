(* Term *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let fresh_binding ?patt:(patt = PattTest.fresh_patt_ground ()) ?ty:(ty = Mono.ty_bool) ?value:(value = ExprTest.fresh_expr_atom ()) _ =
  Mono.binding patt ty value

let fresh_term_expr ?expr:(expr = ExprTest.fresh_expr_atom ()) _ =
  Mono.term_expr expr

let fresh_term_let ?binding:(binding = fresh_binding ()) ?scope:(scope = fresh_term_expr ()) _ =
  Mono.term_let binding scope

(* Assertions *)

(* let binding_not_equal = TestUtils.not_equal "Bindings" Mono.pp_binding *)
let term_not_equal = TestUtils.not_equal "Term values" Mono.pp_term

let assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.Binding expected, Mono.Binding actual ->
    PattTest.assert_patt_equal ~ctxt expected.patt actual.patt;
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty;
    ExprTest.assert_expr_equal ~ctxt expected.value actual.value

let rec assert_term_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.TermLet expected, Mono.TermLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding;
    assert_term_equal ~ctxt expected.scope actual.scope
  | Mono.TermExpr expected, Mono.TermExpr actual ->
    ExprTest.assert_expr_equal ~ctxt expected.expr actual.expr
  | expected, actual -> term_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

(* Terms *)

let test_term_let ctxt =
  let binding = fresh_binding () in
  let scope = fresh_term_expr () in
  let expected = Mono.term_let binding scope in
  match expected with
    | Mono.TermLet actual ->
      assert_binding_equal ~ctxt binding actual.binding;
      assert_term_equal ~ctxt scope actual.scope
    | actual -> term_not_equal ~ctxt expected actual

let test_term_expr ctxt =
  let expr = ExprTest.fresh_expr_atom () in
  let expected = Mono.term_expr expr in
  match expected with
    | Mono.TermExpr actual ->
      ExprTest.assert_expr_equal ~ctxt expr actual.expr
    | actual -> term_not_equal ~ctxt expected actual

(* Bindings *)

let test_binding ctxt =
  let patt = PattTest.fresh_patt_ground () in
  let ty = Mono.ty_bool in
  let value = ExprTest.fresh_expr_atom () in
  let expected = Mono.binding patt ty value in
  match expected with
    | Mono.Binding actual ->
      PattTest.assert_patt_equal ~ctxt patt actual.patt;
      TypeTest.assert_ty_equal ~ctxt ty actual.ty;
      ExprTest.assert_expr_equal ~ctxt value actual.value

let test_constructors =
  "Constructors" >::: [
    "Terms" >::: [
      "Let Bindings" >:: test_term_let;
      "Expressions"  >:: test_term_expr;
    ];
    "Bindings" >:: test_binding;
  ]

(* Pretty Printing *)

let assert_pp_term = PrettyTest.assert_pp Mono.pp_term
let assert_pp_binding = PrettyTest.assert_pp Mono.pp_binding

(* Bindings *)

let test_pp_binding ctxt =
  let patt = PattTest.fresh_patt_ground () in
  let ty = Mono.ty_bool in
  let value =
    let atom = AtomTest.fresh_atom_bool ~value:true () in
    ExprTest.fresh_expr_atom ~atom ()
  in
  fresh_binding ~patt ~ty ~value ()
    |> assert_pp_binding ~ctxt [
         fprintf str_formatter "%a: %a = %a" Mono.pp_patt patt Mono.pp_ty ty Mono.pp_expr value
           |> flush_str_formatter
       ]

(* Terms *)

let test_pp_term_let ctxt =
  let binding = fresh_binding () in
  let scope = fresh_term_expr () in
  fresh_term_let ~binding ~scope ()
    |> assert_pp_term ~ctxt [
         fprintf str_formatter "let %a in %a" Mono.pp_binding binding Mono.pp_term scope
           |> flush_str_formatter
       ]

let test_pp_term_expr ctxt =
  let atom = AtomTest.fresh_atom_bool ~value:true () in
  let expr = ExprTest.fresh_expr_atom ~atom () in
  fresh_term_expr ~expr ()
    |> assert_pp_term ~ctxt ["true"]

let test_pp =
  "Pretty Printing" >::: [
    "Terms" >::: [
      "Let Bindings" >:: test_pp_term_let;
      "Expressions"  >:: test_pp_term_expr;
    ];
    "Bindings" >:: test_pp_binding;
  ]

(* Type Checking *)

(* Bindings *)

let test_check_binding ctxt =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let ty = Mono.ty_bool in
  let binding =
    let patt = PattTest.fresh_patt_var ~id () in
    let value =
      let atom = AtomTest.fresh_atom_bool () in
      ExprTest.fresh_expr_atom ~atom ()
    in
    fresh_binding ~patt ~ty ~value ()
  in
  Mono.check_binding env binding (fun env ->
    MonoUtils.assert_ty_bound ~ctxt id env ty)

let test_check_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let inferred = Mono.ty_bool in
  let annotated = Mono.ty_int in
  let binding =
    let patt =
      let id = SymTest.fresh_sym () in
      PattTest.fresh_patt_var ~id ()
    in
    let value =
      let atom = AtomTest.fresh_atom_bool () in
      ExprTest.fresh_expr_atom ~atom ()
    in
    Mono.binding patt annotated value
  in
  let exn = Mono.MismatchedTypes (inferred, annotated) in
  assert_raises exn (fun _ ->
    Mono.check_binding env binding (fun _ ->
      assert_failure "Expected exception"))

(* Terms *)

let test_check_term_let ctxt =
  let env = EnvTest.fresh () in
  let ty = Mono.ty_bool in
  let term =
    let id = SymTest.fresh_sym () in
    let binding =
      let patt = PattTest.fresh_patt_var ~id () in
      let value =
        let atom = AtomTest.fresh_atom_bool () in
        ExprTest.fresh_expr_atom ~atom ()
      in
      fresh_binding ~patt ~ty ~value ()
    in
    let scope =
      let atom = AtomTest.fresh_atom_ident ~id () in
      let expr = ExprTest.fresh_expr_atom ~atom () in
      fresh_term_expr ~expr ()
    in
    fresh_term_let ~binding ~scope ()
  in
  Mono.check_term env term (fun _ inferred ->
    TypeTest.assert_ty_equal ~ctxt ty inferred)

let test_check_term_expr ctxt =
  let env = EnvTest.fresh () in
  let term =
    let atom = AtomTest.fresh_atom_bool () in
    let expr = ExprTest.fresh_expr_atom ~atom () in
    fresh_term_expr ~expr ()
  in
  Mono.check_term env term (fun _ ty ->
    TypeTest.assert_ty_equal ~ctxt Mono.ty_bool ty)

let test_check =
  "Type Checking" >::: [
    "Bindings" >::: [
      "Valid"            >:: test_check_binding;
      "Mismatched Types" >:: test_check_binding_mismatched_types;
    ];
    "Terms" >::: [
      "Let Expressions" >:: test_check_term_let;
      "Expressions"     >:: test_check_term_expr;
    ];
  ]

(* Test Suite *)

let suite =
  "Terms" >::: [
    test_constructors;
    test_pp;
    test_check;
  ]