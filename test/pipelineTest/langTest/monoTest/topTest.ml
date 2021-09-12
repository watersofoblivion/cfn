(* Top-Level Expressions *)

open Format

open OUnit2

open CommonTest

(* Fixtures *)

let fresh_top_let ?binding:(binding = TermTest.fresh_binding ()) _ =
  Mono.top_let binding

(* Assertions *)

(* let top_not_equal = TestUtils.not_equal "Top-level expressions" Mono.pp_top *)

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Mono.TopLet expected, Mono.TopLet actual ->
    TermTest.assert_binding_equal ~ctxt expected.binding actual.binding

(* Tests *)

(* Constructors *)

let test_top_let ctxt =
  let binding = TermTest.fresh_binding () in
  let expected = Mono.top_let binding in
  match expected with
    | Mono.TopLet actual ->
      TermTest.assert_binding_equal ~ctxt binding actual.binding

let test_constructors =
  "Constructors" >::: [
    "Let Bindings" >:: test_top_let;
  ]

(* Pretty Printing *)

let assert_pp_top = PrettyTest.assert_pp Mono.pp_top

let test_pp_top_let ctxt =
  let binding = TermTest.fresh_binding () in
  Mono.top_let binding
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Mono.pp_binding binding |> flush_str_formatter
       ]

let test_pp =
  "Pretty Printing" >::: [
    "Let Bindings" >:: test_pp_top_let;
  ]

(* Type Checking *)

let test_check_top_let ctxt =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let ty = Mono.ty_bool in
  let top =
    let binding =
      let patt = PattTest.fresh_patt_var ~id () in
      let value =
        let atom = AtomTest.fresh_atom_bool () in
        ExprTest.fresh_expr_atom ~atom ()
      in
      TermTest.fresh_binding ~patt ~ty ~value ()
    in
    fresh_top_let ~binding ()
  in
  Mono.check_top env top (fun env ->
    MonoUtils.assert_ty_bound ~ctxt id env ty)

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
