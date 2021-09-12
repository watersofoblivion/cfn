open Format

open OUnit2

open CommonTest

(* Fixtures *)

let fresh_top_let ?loc:(loc = LocTest.gen ()) ?binding:(binding = ExprTest.fresh_value_binding ()) _ =
  Syntax.top_let loc binding

let fresh_top_val ?loc:(loc = LocTest.gen ()) ?binding:(binding = ExprTest.fresh_value_binding ()) _ =
  Syntax.top_val loc binding

(* Utilities *)

(* Location Stripping *)

let rec deloc_top = function
  | Syntax.TopLet top -> deloc_top_let top.binding
  | Syntax.TopVal top -> deloc_top_val top.binding

and deloc_top_let binding =
  binding
    |> ExprTest.deloc_binding
    |> Syntax.top_let LocTest.dummy

and deloc_top_val binding =
  binding
    |> ExprTest.deloc_binding
    |> Syntax.top_val LocTest.dummy

(* Assertions *)

let top_not_equal = TestUtils.not_equal "Top-level expressions" Syntax.pp_top

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.TopLet expected, Syntax.TopLet actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    ExprTest.assert_binding_equal ~ctxt expected.binding actual.binding
  | Syntax.TopVal expected, Syntax.TopVal actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    ExprTest.assert_binding_equal ~ctxt expected.binding actual.binding
  | expected, actual -> top_not_equal ~ctxt expected actual

(* Constructors *)

let test_top_let ctxt =
  let loc = LocTest.gen () in
  let binding = ExprTest.fresh_value_binding () in
  let expected = Syntax.top_let loc binding in
  match expected with
    | Syntax.TopLet actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      ExprTest.assert_binding_equal ~ctxt binding actual.binding
    | actual -> top_not_equal ~ctxt expected actual

let test_top_val ctxt =
  let loc = LocTest.gen () in
  let binding = ExprTest.fresh_value_binding () in
  let expected = Syntax.top_val loc binding in
  match expected with
    | Syntax.TopVal actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      ExprTest.assert_binding_equal ~ctxt binding actual.binding
    | actual -> top_not_equal ~ctxt expected actual

let test_constructor =
  "Constructors" >::: [
    "Let Bindings" >:: test_top_let;
    "Val Bindings" >:: test_top_val;
  ]

(* Locations *)

let assert_loc_top = SyntaxUtils.assert_loc Syntax.loc_top

let test_loc_top_let = assert_loc_top (fun loc -> fresh_top_let ~loc ())
let test_loc_top_val = assert_loc_top (fun loc -> fresh_top_val ~loc ())

let test_loc =
  "Locations" >::: [
    "Let Bindings" >:: test_loc_top_let;
    "Val Bindings" >:: test_loc_top_val;
  ]

(* Pretty Printing *)

let assert_pp_top = PrettyTest.assert_pp Syntax.pp_top

let test_pp_top_let ctxt =
  let id = "testId" in

  let patt =
    let id = SymTest.fresh_sym ~id () in
    PattTest.fresh_patt_var ~id ()
  in
  let value = ExprTest.fresh_expr_bool ~value:true () in
  let binding = ExprTest.fresh_value_binding ~patt ~value () in

  fresh_top_let ~binding ()
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Syntax.pp_binding binding |> flush_str_formatter
       ]

let test_pp_top_val ctxt =
  let id = "testId" in

  let patt =
    let id = SymTest.fresh_sym ~id () in
    PattTest.fresh_patt_var ~id ()
  in
  let value = ExprTest.fresh_expr_bool ~value:true () in
  let binding = ExprTest.fresh_value_binding ~patt ~value () in

  fresh_top_val ~binding ()
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "val %a" Syntax.pp_binding binding |> flush_str_formatter
       ]

let test_pp =
  "Pretty Printing" >::: [
    "Let Bindings" >:: test_pp_top_let;
    "Val Bindings" >:: test_pp_top_val;
  ]

(* Test Suite *)

let suite =
  "Top-Level Expressions" >::: [
    test_constructor;
    test_loc;
    test_pp;
  ]
