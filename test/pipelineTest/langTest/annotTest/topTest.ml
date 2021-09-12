open Format

open OUnit2

open CommonTest

(* Fixtures *)

let fresh_top_let ?binding:(binding = ExprTest.fresh_binding ()) _ =
  Annot.top_let binding

(* Assertions *)

(* let top_not_equal = TestUtils.not_equal "Top-level expressions" Fmt.top *)

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.TopLet expected, Annot.TopLet actual ->
    ExprTest.assert_binding_equal ~ctxt expected.binding actual.binding

(* Constructors *)

let test_top_let ctxt =
  let binding =
    true
      |> Annot.expr_bool
      |> Annot.binding Annot.patt_ground Annot.ty_bool
  in
  let expected = Annot.top_let binding in
  match expected with
    | Annot.TopLet actual ->
      ExprTest.assert_binding_equal ~ctxt binding actual.binding

let test_constructors =
  "Constructors" >::: [
    "Let Bindings" >:: test_top_let;
  ]

(* Pretty Printing *)

let assert_pp_top = PrettyTest.assert_pp Annot.pp_top

let test_pp_top_let ctxt =
  let binding = ExprTest.fresh_binding () in
  fresh_top_let ~binding ()
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Annot.pp_binding binding
           |> flush_str_formatter
       ]

let test_pp =
  "Pretty Printing" >::: [
    "Let Bindings" >:: test_pp_top_let;
  ]

(* Type Checking *)

let test_check_top_let ctxt =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let ty = Annot.ty_bool in
  let top =
    let patt = PattTest.fresh_patt_var ~id () in
    let value = ExprTest.fresh_expr_bool () in
    let binding = ExprTest.fresh_binding ~patt ~ty ~value () in
    fresh_top_let ~binding ()
  in
  Annot.check_top env top (fun env ->
    AnnotUtils.assert_ty_bound ~ctxt id env ty)

let test_check =
  "Type Checking" >::: [
    "Let Binding" >:: test_check_top_let;
  ]

(* Test Suite *)

let suite =
  "Top-Level Expressions" >::: [
    test_constructors;
    test_pp;
    test_check;
  ]
