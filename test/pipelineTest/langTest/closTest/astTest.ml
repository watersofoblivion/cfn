open Format

open OUnit2

open Common

open CommonTest

(* Assertions *)

let atom_not_equal = TestUtils.not_equal "Atomic values" Clos.pp_atom
let expr_not_equal = TestUtils.not_equal "Expressions" Clos.pp_expr
let block_not_equal = TestUtils.not_equal "Block values" Clos.pp_block
(* let binding_not_equal = TestUtils.not_equal "Bindings" Clos.pp_binding *)
(* let top_not_equal = TestUtils.not_equal "Top-level expressions" Clos.pp_top *)

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

let assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Clos.ExprBuiltin expected, Clos.ExprBuiltin actual ->
    BuiltinTest.assert_builtin_equal ~ctxt expected.fn actual.fn;
    List.iter2 (assert_atom_equal ~ctxt) expected.args actual.args
  | Clos.ExprAtom expected, Clos.ExprAtom actual ->
    assert_atom_equal ~ctxt expected.atom actual.atom
  | expected, actual -> expr_not_equal ~ctxt expected actual

let assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Clos.Binding expected, Clos.Binding actual ->
    PattTest.assert_patt_equal ~ctxt expected.patt actual.patt;
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty;
    assert_expr_equal ~ctxt expected.value actual.value

let rec assert_block_equal ~ctxt expected actual = match (expected, actual) with
  | Clos.BlockLet expected, Clos.BlockLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding;
    assert_block_equal ~ctxt expected.scope actual.scope
  | Clos.BlockExpr expected, Clos.BlockExpr actual ->
    assert_expr_equal ~ctxt expected.expr actual.expr
  | expected, actual -> block_not_equal ~ctxt expected actual

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Clos.TopLet expected, Clos.TopLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding

(* Constructors *)

(* Atoms *)

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

(* Expressions *)

let test_expr_atom ctxt =
  let atom = Clos.atom_bool true in
  let expected = Clos.expr_atom atom in
  match expected with
    | Clos.ExprAtom actual ->
      assert_atom_equal ~ctxt atom actual.atom
    | actual -> expr_not_equal ~ctxt expected actual

(* Bindings *)

let test_binding ctxt =
  let patt = Clos.patt_ground in
  let ty = Clos.ty_bool in
  let value =
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
  in
  let expected = Clos.binding patt ty value in
  match expected with
    | Clos.Binding actual ->
      PattTest.assert_patt_equal ~ctxt patt actual.patt;
      TypeTest.assert_ty_equal ~ctxt ty actual.ty;
      assert_expr_equal ~ctxt value actual.value

(* Blocks *)

let test_block_expr ctxt =
  let expr =
    Clos.atom_bool true
      |> Clos.expr_atom
  in
  let expected = Clos.block_expr expr in
  match expected with
    | Clos.BlockExpr actual ->
      assert_expr_equal ~ctxt expr actual.expr
    | actual -> block_not_equal ~ctxt expected actual

(* Top-Level Expressions *)

let test_top_let ctxt =
  let binding =
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
      |> Clos.binding Clos.patt_ground Clos.ty_bool
  in
  let expected = Clos.top_let binding in
  match expected with
    | Clos.TopLet actual ->
      assert_binding_equal ~ctxt binding actual.binding

let test_constructors =
  "Constructors" >::: [
    "Atoms" >::: [
      "Booleans"    >:: test_atom_bool;
      "Integers"    >:: test_atom_int;
      "Longs"       >:: test_atom_long;
      "Floats"      >:: test_atom_float;
      "Doubles"     >:: test_atom_double;
      "Runes"       >:: test_atom_rune;
      "Strings"     >:: test_atom_string;
      "Identifiers" >:: test_atom_ident;
    ];
    "Expressions" >::: [
      "Atoms" >:: test_expr_atom;
    ];
    "Bindings" >:: test_binding;
    "Blocks" >::: [
      "Expressions" >:: test_block_expr;
    ];
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_top_let;
    ];
  ]

(* Pretty Printing *)

let assert_pp_atom = PrettyTest.assert_pp Clos.pp_atom
let assert_pp_expr = PrettyTest.assert_pp Clos.pp_expr
let assert_pp_block = PrettyTest.assert_pp Clos.pp_block
let assert_pp_binding = PrettyTest.assert_pp Clos.pp_binding
let assert_pp_top = PrettyTest.assert_pp Clos.pp_top

(* Atoms *)

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

(* Expressions *)

let test_pp_expr_atom ctxt =
  true
    |> Clos.atom_bool
    |> Clos.expr_atom
    |> assert_pp_expr ~ctxt ["true"]

(* Bindings *)

let test_pp_binding ctxt =
  let patt = Clos.patt_ground in
  let ty = Clos.ty_bool in
  let value =
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
  in
  Clos.binding patt ty value
    |> assert_pp_binding ~ctxt [
         fprintf str_formatter "%a: %a = %a" Clos.pp_patt patt Clos.pp_ty ty Clos.pp_expr value |> flush_str_formatter
       ]

(* Blocks *)

let test_pp_block_expr ctxt =
  true
    |> Clos.atom_bool
    |> Clos.expr_atom
    |> Clos.block_expr
    |> assert_pp_block ~ctxt ["true"]

(* Top-Level Expressions *)

let test_pp_top_let ctxt =
  let binding =
    let patt = Clos.patt_ground in
    let ty = Clos.ty_bool in
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
      |> Clos.binding patt ty
  in
  Clos.top_let binding
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Clos.pp_binding binding |> flush_str_formatter
       ]

let test_pp =
  "Constructors" >::: [
    "Atoms" >::: [
      "Booleans"    >:: test_pp_atom_bool;
      "Integers"    >:: test_pp_atom_int;
      "Longs"       >:: test_pp_atom_long;
      "Floats"      >:: test_pp_atom_float;
      "Doubles"     >:: test_pp_atom_double;
      "Runes"       >:: test_pp_atom_rune;
      "Strings"     >:: test_pp_atom_string;
      "Identifiers" >:: test_pp_atom_ident;
    ];
    "Expressions" >::: [
      "Atoms" >:: test_pp_expr_atom;
    ];
    "Bindings" >:: test_pp_binding;
    "Blocks" >::: [
      "Expressions" >:: test_pp_block_expr;
    ];
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_pp_top_let;
    ];
  ]

(* Type Checking *)

(* Atoms *)

let test_check_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let b = Clos.atom_bool true in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_bool
    |> Clos.check_atom env b

let test_check_atom_int ctxt =
  let env = EnvTest.fresh () in
  let i = Clos.atom_int 42l in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_int
    |> Clos.check_atom env i

let test_check_atom_long ctxt =
  let env = EnvTest.fresh () in
  let l = Clos.atom_long 42L in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_long
    |> Clos.check_atom env l

let test_check_atom_float ctxt =
  let env = EnvTest.fresh () in
  let f = Clos.atom_float 4.2 in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_float
    |> Clos.check_atom env f

let test_check_atom_double ctxt =
  let env = EnvTest.fresh () in
  let d = Clos.atom_double 4.2 in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_double
    |> Clos.check_atom env d

let test_check_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let r =
    'a'
      |> Uchar.of_char
      |> Clos.atom_rune
  in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_rune
    |> Clos.check_atom env r

let test_check_atom_string ctxt =
  let env = EnvTest.fresh () in
  let s = Clos.atom_string "foo bar" in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_string
    |> Clos.check_atom env s

let test_check_atom_ident ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Clos.atom_ident id in
  let ty = Clos.ty_bool in
  Env.bind id ty env (fun env ->
    TypeTest.assert_ty_equal ~ctxt ty
      |> Clos.check_atom env ident)

let test_check_atom_ident_unbound _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ident = Clos.atom_ident id in
  let exn = Clos.UnboundIdentifier id in
  assert_raises exn (fun _ ->
    Clos.check_atom env ident (fun _ ->
      assert_failure "Expected exception"))

(* Expressions *)

let test_check_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let expr =
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
  in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_bool
    |> Clos.check_expr env expr

(* Bindings *)

let test_check_binding ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Clos.patt_var id in
  let ty = Clos.ty_bool in
  let value =
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
  in
  let binding = Clos.binding patt ty value in
  Clos.check_binding env binding (fun env ->
    ClosUtils.assert_ty_bound ~ctxt id env ty)

let test_check_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let patt = Clos.patt_var id in
  let inferred = Clos.ty_bool in
  let annotated = Clos.ty_int in
  let value =
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
  in
  let binding = Clos.binding patt annotated value in
  let exn = Clos.MismatchedTypes (inferred, annotated) in
  assert_raises exn (fun _ ->
    Clos.check_binding env binding (fun _ ->
      assert_failure "Expected exception"))

(* Blocks *)

let test_check_block_expr ctxt =
  let env = EnvTest.fresh () in
  let block =
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
      |> Clos.block_expr
  in
  Clos.check_block env block (fun _ ty ->
    TypeTest.assert_ty_equal ~ctxt Clos.ty_bool ty)

(* Top-Level Expressions *)

let test_check_top_let ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Clos.ty_bool in
  let top =
    let patt = Clos.patt_var id in
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
      |> Clos.binding patt ty
      |> Clos.top_let
  in
  Clos.check_top env top (fun env ->
    ClosUtils.assert_ty_bound ~ctxt id env ty)

let test_check =
  "Type Checking" >::: [
    "Atoms" >::: [
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
    ];
    "Expressions" >::: [
      "Atoms" >:: test_check_expr_atom;
    ];
    "Bindings" >::: [
      "Valid"            >:: test_check_binding;
      "Mismatched Types" >:: test_check_binding_mismatched_types;
    ];
    "Blocks" >::: [
      "Expressions" >:: test_check_block_expr;
    ];
    "Top-Levels" >::: [
      "Let Binding" >:: test_check_top_let;
    ]
  ]

(* Test Suite *)

let suite =
  "Abstract Syntax" >::: [
    test_constructors;
    test_pp;
    test_check;
  ]
