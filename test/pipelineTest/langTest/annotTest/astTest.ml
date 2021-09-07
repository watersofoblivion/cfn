open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_expr_bool ?value:(value = true) _ =
  Annot.expr_bool value

let fresh_expr_int ?value:(value = 42l) _ =
  Annot.expr_int value

let fresh_expr_long ?value:(value = 42L) _ =
  Annot.expr_long value

let fresh_expr_float ?value:(value = 4.2) _ =
  Annot.expr_float value

let fresh_expr_double ?value:(value = 4.2) _ =
  Annot.expr_double value

let fresh_expr_rune ?value:(value = 'a') _ =
  value
    |> Uchar.of_char
    |> Annot.expr_rune

let fresh_expr_string ?value:(value = "foo bar") _ =
  Annot.expr_string value

let fresh_expr_ident ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  seq
    |> Sym.gen ~id
    |> Annot.expr_ident

let fresh_expr_builtin ?fn:(fn = BuiltinTest.fresh_builtin ()) ?args:(args = [fresh_expr_bool (); fresh_expr_bool ()]) _ =
  Annot.expr_builtin fn args

let rec fresh_expr_let ?binding:(binding = fresh_binding ()) ?scope:(scope = fresh_expr_bool ()) _ =
  Annot.expr_let binding scope

and fresh_binding ?patt:(patt = PattTest.fresh_patt_ground ()) ?ty:(ty = Annot.ty_bool) ?value:(value = fresh_expr_bool ()) _ =
  Annot.binding patt ty value

let fresh_top_let ?binding:(binding = fresh_binding ()) _ =
  Annot.top_let binding

(* Assertions *)

let expr_not_equal = TestUtils.not_equal "Expressions" Annot.pp_expr
(* let binding_not_equal = TestUtils.not_equal "Bindings" Fmt.binding *)
(* let top_not_equal = TestUtils.not_equal "Top-level expressions" Fmt.top *)

let rec assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.ExprBool expected, Annot.ExprBool actual ->
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Annot.ExprInt expected, Annot.ExprInt actual ->
    assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" expected.value actual.value
  | Annot.ExprLong expected, Annot.ExprLong actual ->
    assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" expected.value actual.value
  | Annot.ExprFloat expected, Annot.ExprFloat actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" expected.value actual.value
  | Annot.ExprDouble expected, Annot.ExprDouble actual ->
    assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" expected.value actual.value
  | Annot.ExprRune expected, Annot.ExprRune actual ->
    let printer c = sprintf "%c" (Uchar.to_char c) in
    assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" expected.value actual.value
  | Annot.ExprString expected, Annot.ExprString actual ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" expected.value actual.value
  | Annot.ExprIdent expected, Annot.ExprIdent actual ->
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | Annot.ExprBuiltin expected, Annot.ExprBuiltin actual ->
    BuiltinTest.assert_builtin_equal ~ctxt expected.fn actual.fn;
    List.iter2 (assert_expr_equal ~ctxt) expected.args actual.args
  | Annot.ExprLet expected, Annot.ExprLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding;
    assert_expr_equal ~ctxt expected.scope actual.scope
  | expected, actual -> expr_not_equal ~ctxt expected actual

and assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.Binding expected, Annot.Binding actual ->
    PattTest.assert_patt_equal ~ctxt expected.patt actual.patt;
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty;
    assert_expr_equal ~ctxt expected.value actual.value

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Annot.TopLet expected, Annot.TopLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding

(* Constructors *)

let test_expr_bool ctxt =
  let value = true in
  let expected = Annot.expr_bool value in
  match expected with
    | Annot.ExprBool actual ->
      assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_int ctxt =
  let value = 42l in
  let expected = Annot.expr_int value in
  match expected with
    | Annot.ExprInt actual ->
      assert_equal ~ctxt ~cmp:Int32.equal ~printer:Int32.to_string ~msg:"Integer values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_long ctxt =
  let value = 42L in
  let expected = Annot.expr_long value in
  match expected with
    | Annot.ExprLong actual ->
      assert_equal ~ctxt ~cmp:Int64.equal ~printer:Int64.to_string ~msg:"Long values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_float ctxt =
  let value = 4.2 in
  let expected = Annot.expr_float value in
  match expected with
    | Annot.ExprFloat actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Float values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_double ctxt =
  let value = 4.2 in
  let expected = Annot.expr_double value in
  match expected with
    | Annot.ExprDouble actual ->
      assert_equal ~ctxt ~printer:string_of_float ~msg:"Double values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_rune ctxt =
  let value = Uchar.of_char 'a' in
  let expected = Annot.expr_rune value in
  match expected with
    | Annot.ExprRune actual ->
      let printer c = sprintf "%c" (Uchar.to_char c) in
      assert_equal ~ctxt ~cmp:Uchar.equal ~printer ~msg:"Rune values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_string ctxt =
  let value = "foobar" in
  let expected = Annot.expr_string value in
  match expected with
    | Annot.ExprString actual ->
      assert_equal ~ctxt ~printer:Fun.id ~msg:"String values are not equal" value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_ident ctxt =
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Annot.expr_ident id in
  match expected with
    | Annot.ExprIdent actual ->
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> expr_not_equal ~ctxt expected actual

let test_binding ctxt =
  let patt = Annot.patt_ground in
  let ty = Annot.ty_bool in
  let value = Annot.expr_bool true in
  let expected = Annot.binding patt ty value in
  match expected with
    | Annot.Binding actual ->
      PattTest.assert_patt_equal ~ctxt patt actual.patt;
      TypeTest.assert_ty_equal ~ctxt ty actual.ty;
      assert_expr_equal ~ctxt value actual.value

let test_top_let ctxt =
  let binding =
    true
      |> Annot.expr_bool
      |> Annot.binding Annot.patt_ground Annot.ty_bool
  in
  let expected = Annot.top_let binding in
  match expected with
    | Annot.TopLet actual ->
      assert_binding_equal ~ctxt binding actual.binding

let test_constructors =
  "Constructors" >::: [
    "Expressions" >::: [
      "Booleans"    >:: test_expr_bool;
      "Integers"    >:: test_expr_int;
      "Longs"       >:: test_expr_long;
      "Floats"      >:: test_expr_float;
      "Doubles"     >:: test_expr_double;
      "Runes"       >:: test_expr_rune;
      "Strings"     >:: test_expr_string;
      "Identifiers" >:: test_expr_ident;
    ];
    "Bindings" >:: test_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_top_let;
    ];
  ]

(* Pretty Printing *)

let assert_pp_expr = PrettyTest.assert_pp Annot.pp_expr
let assert_pp_binding = PrettyTest.assert_pp Annot.pp_binding
let assert_pp_top = PrettyTest.assert_pp Annot.pp_top

let test_pp_expr_bool ctxt =
  Annot.expr_bool true
    |> assert_pp_expr ~ctxt ["true"];
  Annot.expr_bool false
    |> assert_pp_expr ~ctxt ["false"]

let test_pp_expr_int ctxt =
  Annot.expr_int 42l
    |> assert_pp_expr ~ctxt ["42"]

let test_pp_expr_long ctxt =
  Annot.expr_long 42L
    |> assert_pp_expr ~ctxt ["42"]

let test_pp_expr_float ctxt =
  Annot.expr_float 4.2
    |> assert_pp_expr ~ctxt ["4.2"]

let test_pp_expr_double ctxt =
  Annot.expr_double 4.2
    |> assert_pp_expr ~ctxt ["4.2"]

let test_pp_expr_rune ctxt =
  'a'
    |> Uchar.of_char
    |> Annot.expr_rune
    |> assert_pp_expr ~ctxt ["'a'"]

let test_pp_expr_string ctxt =
  Annot.expr_string "foo bar"
    |> assert_pp_expr ~ctxt ["\"foo bar\""]

let test_pp =
  "Pretty Printing" >::: [
    "Expressions" >::: [
      "Booleans" >:: test_pp_expr_bool;
      "Integers" >:: test_pp_expr_int;
      "Longs"    >:: test_pp_expr_long;
      "Floats"   >:: test_pp_expr_float;
      "Doubles"  >:: test_pp_expr_double;
      "Runes"    >:: test_pp_expr_rune;
      "Strings"  >:: test_pp_expr_string;
    ];
    (* "Bindings" >:: test_pp_binding; *)
    (* "Top-Level Expressions" >::: [ *)
      (* "Let Bindings" >:: test_pp_top_let; *)
    (* ]; *)
  ]

(* Type Checking *)

let test_check_expr_bool ctxt =
  let env = EnvTest.fresh () in
  let b = Annot.expr_bool true in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_bool
    |> Annot.check_expr env b

let test_check_expr_int ctxt =
  let env = EnvTest.fresh () in
  let i = Annot.expr_int 42l in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_int
    |> Annot.check_expr env i

let test_check_expr_long ctxt =
  let env = EnvTest.fresh () in
  let l = Annot.expr_long 42L in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_long
    |> Annot.check_expr env l

let test_check_expr_float ctxt =
  let env = EnvTest.fresh () in
  let f = Annot.expr_float 4.2 in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_float
    |> Annot.check_expr env f

let test_check_expr_double ctxt =
  let env = EnvTest.fresh () in
  let d = Annot.expr_double 4.2 in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_double
    |> Annot.check_expr env d

let test_check_expr_rune ctxt =
  let env = EnvTest.fresh () in
  let r =
    'a'
      |> Uchar.of_char
      |> Annot.expr_rune
  in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_rune
    |> Annot.check_expr env r

let test_check_expr_string ctxt =
  let env = EnvTest.fresh () in
  let s = Annot.expr_string "foo bar" in
  TypeTest.assert_ty_equal ~ctxt Annot.ty_string
    |> Annot.check_expr env s

let test_check_expr_ident ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Annot.expr_ident id in
  let ty = Annot.ty_bool in
  Env.bind id ty env (fun env ->
    TypeTest.assert_ty_equal ~ctxt ty
      |> Annot.check_expr env ident)

let test_check_expr_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Annot.expr_ident id in
  let exn = Annot.UnboundIdentifier id in
  assert_raises exn (fun _ ->
    Annot.check_expr env ident (fun _ ->
      assert_failure "Expected exception"))

let test_check_binding ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Annot.patt_var id in
  let ty = Annot.ty_bool in
  let value = Annot.expr_bool true in
  let binding = Annot.binding patt ty value in
  Annot.check_binding env binding (fun env ->
    AnnotUtils.assert_ty_bound ~ctxt id env ty)

let test_check_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Annot.patt_var id in
  let inferred = Annot.ty_bool in
  let annotated = Annot.ty_int in
  let value = Annot.expr_bool true in
  let binding = Annot.binding patt annotated value in
  let exn = Annot.MismatchedTypes (value, inferred, annotated) in
  assert_raises exn (fun _ ->
    Annot.check_binding env binding (fun _ ->
      assert_failure "Expected exception"))

let test_check_top_let ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Annot.ty_bool in
  let top =
    let patt = Annot.patt_var id in
    true
      |> Annot.expr_bool
      |> Annot.binding patt ty
      |> Annot.top_let
  in
  Annot.check_top env top (fun env ->
    AnnotUtils.assert_ty_bound ~ctxt id env ty)

let test_check =
  "Type Checking" >::: [
    "Expressions" >::: [
      "Booleans"    >:: test_check_expr_bool;
      "Integers"    >:: test_check_expr_int;
      "Longs"       >:: test_check_expr_long;
      "Float"       >:: test_check_expr_float;
      "Double"      >:: test_check_expr_double;
      "Rune"        >:: test_check_expr_rune;
      "String"      >:: test_check_expr_string;
      "Identifiers" >::: [
        "Bound"   >:: test_check_expr_ident;
        "Unbound" >:: test_check_expr_ident_unbound
      ];
    ];
    "Bindings" >::: [
      "Valid"            >:: test_check_binding;
      "Mismatched Types" >:: test_check_binding_mismatched_types;
    ];
    "Top-Levels" >::: [
      "Let Binding" >:: test_check_top_let;
    ];
  ]

(* Test Suite *)

let suite =
  "Abstract Syntax" >::: [
    test_constructors;
    test_pp;
    test_check;
  ]
