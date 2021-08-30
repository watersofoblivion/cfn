open Common

open OUnit2

(* Helpers *)

let fresh ?seq:(seq = Sym.seq ()) _ = Env.env seq

(* Assertions *)

let assert_symbol_of ~ctxt id env expected =
  Env.symbol_of id env
    |> SymTest.assert_sym_equal ~ctxt expected

let assert_constr_of ~ctxt id env expected =
  Env.constr_of id env (fun _ ->
    SymTest.assert_sym_equal ~ctxt expected)

let assert_bound assert_equal ~ctxt sym env expected =
  try
    Env.lookup sym env
      |> assert_equal ~ctxt expected
  with Not_found ->
    assert_failure "Symbol is not bound"

(* Test *)

(* Constructor *)

let test_env _ = ()
let test_prune _ = ()

let test_constr =
  "Contructors" >::: [
    "Environment" >:: test_env;
    "Prune"       >:: test_prune;
  ]

(* Alpha Renaming *)

let test_symbol_of_bound ctxt =
  let id = "the-id" in
  let env = fresh () in
  Env.rename id env (fun env expected ->
    Env.symbol_of id env
      |> SymTest.assert_sym_equal ~ctxt expected)

let test_symbol_of_unbound ctxt =
  let id = "the-id" in
  let expected = () |> Sym.seq |> Sym.gen ~id in
  ()
    |> Sym.seq
    |> Env.env
    |> Env.symbol_of id
    |> SymTest.assert_sym_equal ~ctxt expected

let test_constr_of_bound ctxt =
  let id = "the-id" in
  let env = fresh () in
  Env.constr_of id env (fun env expected ->
    Env.constr_of id env (fun _ ->
      SymTest.assert_sym_equal ~ctxt expected))

let test_constr_of_unbound ctxt =
  let id = "the-id" in
  let env = fresh () in
  let expected = () |> Sym.seq |> Sym.gen ~id in
  Env.constr_of id env (fun _ ->
    SymTest.assert_sym_equal ~ctxt expected)

let test_rename_non_destructive ctxt =
  let env = fresh () in
  let id = "the-id" in
  Env.rename id env (fun env sym ->
    Env.rename id env (fun env' sym' ->
      assert_symbol_of ~ctxt id env sym;
      assert_symbol_of ~ctxt id env' sym'))

let test_rename_separate ctxt =
  let env = fresh () in
  let id = "the-id" in
  Env.rename id env (fun env sym ->
    Env.constr_of id env (fun env constr ->
      assert_symbol_of ~ctxt id env sym;
      assert_constr_of ~ctxt id env constr;
      SymTest.sym_not_equal ~ctxt sym constr))

let test_alpha =
  "Alpha Renaming" >::: [
    "Bind" >::: [
      "Non-Destructive"                   >:: test_rename_non_destructive;
      "Separate Symbols and Constructors" >:: test_rename_separate;
    ];
    "Lookup" >::: [
      "Symbols" >::: [
        "Bound"   >:: test_symbol_of_bound;
        "Unbound" >:: test_symbol_of_unbound;
      ];
      "Constructors" >::: [
        "Bound"   >:: test_constr_of_bound;
        "Unbound" >:: test_constr_of_unbound;
      ];
    ];
  ]

(* Binding *)

let assert_int_bound ~ctxt =
  let assert_equal ~ctxt expected actual =
    assert_equal ~ctxt ~msg:"Values are not equal" ~printer:string_of_int expected actual
  in
  assert_bound assert_equal ~ctxt

let test_lookup_bound ctxt =
  let sym = () |> Sym.seq |> Sym.gen in
  let expected = 42 in
  let env = fresh () in
  Env.bind sym expected env (fun env ->
    assert_int_bound ~ctxt sym env expected)

let test_lookup_unbound _ =
  let sym = () |> Sym.seq |> Sym.gen in
  let env = fresh () in
  assert_raises Not_found (fun _ ->
    Env.lookup sym env)

let test_bind_non_destructive ctxt =
  let sym = () |> Sym.seq |> Sym.gen in
  let masked = 24 in
  let expected = 42 in
  let env = fresh () in
  Env.bind sym masked env (fun env ->
    Env.bind sym expected env (fun env' ->
      assert_int_bound ~ctxt sym env masked;
      assert_int_bound ~ctxt sym env' expected))

let test_binding =
  "Binding" >::: [
    "Bind" >::: [
      "Non-Destructive" >:: test_bind_non_destructive;
    ];
    "Lookup" >::: [
      "Bound"   >:: test_lookup_bound;
      "Unbound" >:: test_lookup_unbound;
    ];
  ]

(* Suite *)

let suite =
  "Environment" >::: [
    test_constr;
    test_alpha;
    test_binding;
  ]
