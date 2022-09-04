(* Atomic Values *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_atom_bool ?value:(value = true) _ =
  Clos.atom_bool value

let fresh_atom_int ?value:(value = 42l) _ =
  Clos.atom_int value

let fresh_atom_long ?value:(value = 42L) _ =
  Clos.atom_long value

let fresh_atom_float ?value:(value = 4.2) _ =
  Clos.atom_float value

let fresh_atom_double ?value:(value = 4.2) _ =
  Clos.atom_double value

let fresh_atom_rune ?value:(value = 'a') _ =
  value
    |> Uchar.of_char
    |> Clos.atom_rune

let fresh_atom_string ?value:(value = "foo bar") _ =
  Clos.atom_string value

let fresh_atom_ident ?id:(id = SymTest.fresh_sym ()) _ =
  Clos.atom_ident id

(* Assertions *)

let atom_not_equal = TestUtils.not_equal "Atomic values" Clos.pp_atom

let assert_atom_equal ~ctxt expected actual = match (expected, actual) with
  | Clos.AtomBool expected, Clos.AtomBool actual ->
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Clos.AtomInt expected, Clos.AtomInt actual ->
    assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" expected.value actual.value
  | Clos.AtomLong expected, Clos.AtomLong actual ->
    assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" expected.value actual.value
  | Clos.AtomFloat expected, Clos.AtomFloat actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" expected.value actual.value
  | Clos.AtomDouble expected, Clos.AtomDouble actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" expected.value actual.value
  | Clos.AtomRune expected, Clos.AtomRune actual ->
    let printer c = sprintf "%c" (Uchar.to_char c) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" expected.value actual.value
  | Clos.AtomString expected, Clos.AtomString actual ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" expected.value actual.value
  | Clos.AtomIdent expected, Clos.AtomIdent actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | expected, actual -> atom_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_atom_bool ctxt =
  let value = true in
  let expected = Clos.atom_bool value in
  match expected with
    | Clos.AtomBool actual ->
      assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_int ctxt =
  let value = 42l in
  let expected = Clos.atom_int value in
  match expected with
    | Clos.AtomInt actual ->
      assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_long ctxt =
  let value = 42L in
  let expected = Clos.atom_long value in
  match expected with
    | Clos.AtomLong actual ->
      assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_float ctxt =
  let value = 4.2 in
  let expected = Clos.atom_float value in
  match expected with
    | Clos.AtomFloat actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_double ctxt =
  let value = 4.2 in
  let expected = Clos.atom_double value in
  match expected with
    | Clos.AtomDouble actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_rune ctxt =
  let value = Uchar.of_char 'a' in
  let expected = Clos.atom_rune value in
  match expected with
    | Clos.AtomRune actual ->
      let printer c = sprintf "%c" (Uchar.to_char c) in
      assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_string ctxt =
  let value = "foobar" in
  let expected = Clos.atom_string value in
  match expected with
    | Clos.AtomString actual ->
      assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" value actual.value
    | actual -> atom_not_equal ~ctxt expected actual

let test_atom_ident ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Clos.atom_ident id in
  match expected with
    | Clos.AtomIdent actual ->
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

let assert_pp_atom = PrettyTest.assert_pp Clos.pp_atom

let test_pp_atom_bool ctxt =
  Clos.atom_bool true
    |> assert_pp_atom ~ctxt ["true"];
  Clos.atom_bool false
    |> assert_pp_atom ~ctxt ["false"]

let test_pp_atom_int ctxt =
  Clos.atom_int 42l
    |> assert_pp_atom ~ctxt ["42"]

let test_pp_atom_long ctxt =
  Clos.atom_long 42L
    |> assert_pp_atom ~ctxt ["42"]

let test_pp_atom_float ctxt =
  Clos.atom_float 4.2
    |> assert_pp_atom ~ctxt ["4.2"]

let test_pp_atom_double ctxt =
  Clos.atom_double 4.2
    |> assert_pp_atom ~ctxt ["4.2"]

let test_pp_atom_rune ctxt =
  'a'
    |> Uchar.of_char
    |> Clos.atom_rune
    |> assert_pp_atom ~ctxt ["'a'"]

let test_pp_atom_string ctxt =
  "foo bar"
    |> Clos.atom_string
    |> assert_pp_atom ~ctxt ["\"foo bar\""]

let test_pp_atom_ident ctxt =
  ()
    |> Sym.seq
    |> Sym.gen
    |> Clos.atom_ident
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
  TypeTest.assert_ty_equal ~ctxt Clos.ty_bool
    |> Clos.check_atom env atom

let test_check_atom_int ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_int () in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_int
    |> Clos.check_atom env atom

let test_check_atom_long ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_long () in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_long
    |> Clos.check_atom env atom

let test_check_atom_float ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_float () in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_float
    |> Clos.check_atom env atom

let test_check_atom_double ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_double () in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_double
    |> Clos.check_atom env atom

let test_check_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_rune () in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_rune
    |> Clos.check_atom env atom

let test_check_atom_string ctxt =
  let env = EnvTest.fresh () in
  let atom = fresh_atom_string () in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_string
    |> Clos.check_atom env atom

let test_check_atom_ident ctxt =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let ty = Clos.ty_bool in
  let atom = fresh_atom_ident ~id () in
  Env.bind id ty env (fun env ->
    TypeTest.assert_ty_equal ~ctxt ty
      |> Clos.check_atom env atom)

let test_check_atom_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let atom = fresh_atom_ident ~id () in
  let exn = Clos.UnboundIdentifier { id } in
  assert_raises exn (fun _ ->
    Clos.check_atom env atom (fun _ ->
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
