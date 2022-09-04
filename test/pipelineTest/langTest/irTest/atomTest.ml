(* Atomic Values *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_atom_bool ?value:(value = true) _ =
  Ir.atom_bool value

let fresh_atom_int ?value:(value = 42l) _ =
  Ir.atom_int value

let fresh_atom_long ?value:(value = 42L) _ =
  Ir.atom_long value

let fresh_atom_float ?value:(value = 4.2) _ =
  Ir.atom_float value

let fresh_atom_double ?value:(value = 4.2) _ =
  Ir.atom_double value

let fresh_atom_rune ?value:(value = 'a') _ =
  value
    |> Uchar.of_char
    |> Ir.atom_rune

let fresh_atom_string ?value:(value = "foo bar") _ =
  Ir.atom_string value

let fresh_atom_ident ?id:(id = SymTest.fresh_sym ()) _ =
  Ir.atom_ident id

(* Assertions *)

let atom_not_equal = TestUtils.not_equal "Atomic values" Ir.pp_atom

let assert_atom_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.AtomBool expected, Ir.AtomBool actual ->
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Ir.AtomInt expected, Ir.AtomInt actual ->
    assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" expected.value actual.value
  | Ir.AtomLong expected, Ir.AtomLong actual ->
    assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" expected.value actual.value
  | Ir.AtomFloat expected, Ir.AtomFloat actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" expected.value actual.value
  | Ir.AtomDouble expected, Ir.AtomDouble actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" expected.value actual.value
  | Ir.AtomRune expected, Ir.AtomRune actual ->
    let printer c = sprintf "%c" (Uchar.to_char c) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" expected.value actual.value
  | Ir.AtomString expected, Ir.AtomString actual ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" expected.value actual.value
  | Ir.AtomIdent expected, Ir.AtomIdent actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> atom_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_atom_bool ctxt =
  let value = true in
  let expected = Ir.atom_bool value in
  match expected with
    | Ir.AtomBool actual ->
      assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_int ctxt =
  let value = 42l in
  let expected = Ir.atom_int value in
  match expected with
    | Ir.AtomInt actual ->
      assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_long ctxt =
  let value = 42L in
  let expected = Ir.atom_long value in
  match expected with
    | Ir.AtomLong actual ->
      assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_float ctxt =
  let value = 4.2 in
  let expected = Ir.atom_float value in
  match expected with
    | Ir.AtomFloat actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_double ctxt =
  let value = 4.2 in
  let expected = Ir.atom_double value in
  match expected with
    | Ir.AtomDouble actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_rune ctxt =
  let value = Uchar.of_char 'a' in
  let expected = Ir.atom_rune value in
  match expected with
    | Ir.AtomRune actual ->
      let printer c = sprintf "%c" (Uchar.to_char c) in
      assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_string ctxt =
  let value = "foobar" in
  let expected = Ir.atom_string value in
  match expected with
    | Ir.AtomString actual ->
      assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_ident ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Ir.atom_ident id in
  match expected with
    | Ir.AtomIdent actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> atom_not_equal ~ctxt expected actual

let test_constructors =
  "Constructors" >::: [
    "Booleans"    >:: test_atom_bool;
    "Integers"    >:: test_atom_int;
    "Longs"       >:: test_atom_long;
    "Floats"      >:: test_atom_float;
    "Doubles"     >:: test_atom_double;
    "Runes"       >:: test_atom_rune;
    "Strings"     >:: test_atom_string;
    "Identifiers" >:: test_atom_ident;
  ]

(* Pretty Printing *)

let assert_pp_atom = PrettyTest.assert_pp Ir.pp_atom

let test_pp_atom_bool ctxt =
  Ir.atom_bool true
    |> assert_pp_atom ~ctxt ["true"];
  Ir.atom_bool false
    |> assert_pp_atom ~ctxt ["false"]

let test_pp_atom_int ctxt =
  Ir.atom_int 42l
    |> assert_pp_atom ~ctxt ["42"]

let test_pp_atom_long ctxt =
  Ir.atom_long 42L
    |> assert_pp_atom ~ctxt ["42"]

let test_pp_atom_float ctxt =
  Ir.atom_float 4.2
    |> assert_pp_atom ~ctxt ["4.2"]

let test_pp_atom_double ctxt =
  Ir.atom_double 4.2
    |> assert_pp_atom ~ctxt ["4.2"]

let test_pp_atom_rune ctxt =
  'a'
    |> Uchar.of_char
    |> Ir.atom_rune
    |> assert_pp_atom ~ctxt ["'a'"]

let test_pp_atom_string ctxt =
  "foo bar"
    |> Ir.atom_string
    |> assert_pp_atom ~ctxt ["\"foo bar\""]

let test_pp_atom_ident ctxt =
  ()
    |> Sym.seq
    |> Sym.gen
    |> Ir.atom_ident
    |> assert_pp_atom ~ctxt ["$0"]

let test_pp =
  "Pretty Printing" >::: [
    "Booleans"    >:: test_pp_atom_bool;
    "Integers"    >:: test_pp_atom_int;
    "Longs"       >:: test_pp_atom_long;
    "Floats"      >:: test_pp_atom_float;
    "Doubles"     >:: test_pp_atom_double;
    "Runes"       >:: test_pp_atom_rune;
    "Strings"     >:: test_pp_atom_string;
    "Identifiers" >:: test_pp_atom_ident;
  ]

(* Type Checking *)

let test_check_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_bool () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_bool
    |> Ir.check_atom env atom

let test_check_atom_int ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_int () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_int
    |> Ir.check_atom env atom

let test_check_atom_long ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_long () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_long
    |> Ir.check_atom env atom

let test_check_atom_float ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_float () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_float
    |> Ir.check_atom env atom

let test_check_atom_double ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_double () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_double
    |> Ir.check_atom env atom

let test_check_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_rune () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_rune
    |> Ir.check_atom env atom

let test_check_atom_string ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_string () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_string
    |> Ir.check_atom env atom

let test_check_atom_ident ctxt =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let ty = Ir.ty_bool in
  let atom = fresh_atom_ident ~id () in
  Env.bind id ty env (fun env ->
    TypeTest.assert_ty_equal ~ctxt ty
      |> Ir.check_atom env atom)

let test_check_atom_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let atom = fresh_atom_ident ~id () in
  let exn = Ir.UnboundIdentifier { id } in
  assert_raises exn (fun _ ->
    Ir.check_atom env atom (fun _ ->
      assert_failure "Expected exception"))

let test_check =
  "Type Checking" >::: [
    "Booleans"    >:: test_check_atom_bool;
    "Integers"    >:: test_check_atom_int;
    "Longs"       >:: test_check_atom_long;
    "Float"       >:: test_check_atom_float;
    "Double"      >:: test_check_atom_double;
    "Rune"        >:: test_check_atom_rune;
    "String"      >:: test_check_atom_string;
    "Identifiers" >::: [
      "Bound"   >:: test_check_atom_ident;
      "Unbound" >:: test_check_atom_ident_unbound
    ];
  ]

(* Test Suite *)

let suite =
  "Atomic Values" >::: [
    test_constructors;
    test_pp;
    test_check;
  ]
