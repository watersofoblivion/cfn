(* Abstract Syntax *)

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

let fresh_expr_builtin ?fn:(fn = BuiltinTest.fresh_builtin_add ()) ?args:(args = [fresh_atom_int (); fresh_atom_int ()]) _ =
  Clos.expr_builtin fn args

let fresh_expr_atom ?atom:(atom = fresh_atom_bool ()) _ =
  Clos.expr_atom atom

let fresh_binding ?patt:(patt = PattTest.fresh_patt_ground ()) ?ty:(ty = Clos.ty_bool) ?value:(value = fresh_expr_atom ()) _ =
  Clos.binding patt ty value

let fresh_block_expr ?expr:(expr = fresh_expr_atom ()) _ =
  Clos.block_expr expr

let fresh_block_let ?binding:(binding = fresh_binding ()) ?scope:(scope = fresh_block_expr ()) _ =
  Clos.block_let binding scope

let fresh_top_let ?binding:(binding = fresh_binding ()) _ =
  Clos.top_let binding

(* Assertions *)

let atom_not_equal = TestUtils.not_equal "Atomic values" Clos.pp_atom
let expr_not_equal = TestUtils.not_equal "Expressions" Clos.pp_expr
(* let binding_not_equal = TestUtils.not_equal "Bindings" Clos.pp_binding *)
let block_not_equal = TestUtils.not_equal "Block values" Clos.pp_block
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

(* Tests *)

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

let test_expr_builtin ctxt =
  let fn = BuiltinTest.fresh_builtin_add () in
  let args = [fresh_atom_int (); fresh_atom_long ()] in
  let expected = Clos.expr_builtin fn args in
  match expected with
    | Clos.ExprBuiltin actual ->
      BuiltinTest.assert_builtin_equal ~ctxt fn actual.fn;
      List.iter2 (assert_atom_equal ~ctxt) args actual.args
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_atom ctxt =
  let atom = fresh_atom_bool () in
  let expected = Clos.expr_atom atom in
  match expected with
    | Clos.ExprAtom actual ->
      assert_atom_equal ~ctxt atom actual.atom
    | actual -> expr_not_equal ~ctxt expected actual

(* Blocks *)

let test_block_let ctxt =
  let binding = fresh_binding () in
  let scope = fresh_block_expr () in
  let expected = Clos.block_let binding scope in
  match expected with
    | Clos.BlockLet actual ->
      assert_binding_equal ~ctxt binding actual.binding;
      assert_block_equal ~ctxt scope actual.scope
    | actual -> block_not_equal ~ctxt expected actual

let test_block_expr ctxt =
  let expr = fresh_expr_atom () in
  let expected = Clos.block_expr expr in
  match expected with
    | Clos.BlockExpr actual ->
      assert_expr_equal ~ctxt expr actual.expr
    | actual -> block_not_equal ~ctxt expected actual

(* Bindings *)

let test_binding ctxt =
  let patt = PattTest.fresh_patt_ground () in
  let ty = Clos.ty_bool in
  let value = fresh_expr_atom () in
  let expected = Clos.binding patt ty value in
  match expected with
    | Clos.Binding actual ->
      PattTest.assert_patt_equal ~ctxt patt actual.patt;
      TypeTest.assert_ty_equal ~ctxt ty actual.ty;
      assert_expr_equal ~ctxt value actual.value

(* Top-Level Expressions *)

let test_top_let ctxt =
  let binding = fresh_binding () in
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
      "Built-in Function Application" >:: test_expr_builtin;
      "Atoms"                         >:: test_expr_atom;
    ];
    "Blocks" >::: [
      "Let Bindings" >:: test_block_let;
      "Expressions"  >:: test_block_expr;
    ];
    "Bindings" >:: test_binding;
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

let test_pp_expr_builtin ctxt =
  let fn = BuiltinTest.fresh_builtin_struct_eq () in
  let args = [
    fresh_atom_bool ();
    fresh_atom_int ();
  ] in
  let pp_sep fmt _ = fprintf fmt "@ " in
  fresh_expr_builtin ~fn ~args ()
    |> assert_pp_expr ~ctxt [
         fprintf str_formatter "%a %a" Clos.pp_builtin fn (pp_print_list ~pp_sep Clos.pp_atom) args
           |> flush_str_formatter;
       ]

let test_pp_expr_atom ctxt =
  let atom = fresh_atom_bool ~value:true () in
  fresh_expr_atom ~atom ()
    |> assert_pp_expr ~ctxt ["true"]

(* Bindings *)

let test_pp_binding ctxt =
  let patt = PattTest.fresh_patt_ground () in
  let ty = Clos.ty_bool in
  let value =
    let atom = fresh_atom_bool ~value:true () in
    fresh_expr_atom ~atom ()
  in
  fresh_binding ~patt ~ty ~value ()
    |> assert_pp_binding ~ctxt [
         fprintf str_formatter "%a: %a = %a" Clos.pp_patt patt Clos.pp_ty ty Clos.pp_expr value
           |> flush_str_formatter
       ]

(* Blocks *)

let test_pp_block_let ctxt =
  let binding = fresh_binding () in
  let scope = fresh_block_expr () in
  fresh_block_let ~binding ~scope ()
    |> assert_pp_block ~ctxt [
         fprintf str_formatter "let %a in %a" Clos.pp_binding binding Clos.pp_block scope
           |> flush_str_formatter
       ]

let test_pp_block_expr ctxt =
  let atom = fresh_atom_bool ~value:true () in
  let expr = fresh_expr_atom ~atom () in
  fresh_block_expr ~expr ()
    |> assert_pp_block ~ctxt ["true"]

(* Top-Level Expressions *)

let test_pp_top_let ctxt =
  let binding = fresh_binding () in
  Clos.top_let binding
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Clos.pp_binding binding |> flush_str_formatter
       ]

let test_pp =
  "Pretty Printing" >::: [
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
      "Built-in Function Application" >:: test_pp_expr_builtin;
      "Atoms"                         >:: test_pp_expr_atom;
    ];
    "Blocks" >::: [
      "Let Bindings" >:: test_pp_block_let;
      "Expressions"  >:: test_pp_block_expr;
    ];
    "Bindings" >:: test_pp_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_pp_top_let;
    ];
  ]

(* Type Checking *)

(* Atoms *)

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
  let exn = Clos.UnboundIdentifier id in
  assert_raises exn (fun _ ->
    Clos.check_atom env atom (fun _ ->
      assert_failure "Expected exception"))

(* Expressions *)

let test_check_expr_builtin_fixed ctxt =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_struct_eq ~ty:Clos.ty_int () in
  let args = [fresh_atom_int (); fresh_atom_int ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_bool
    |> Clos.check_expr env expr

let test_check_expr_builtin_var ctxt =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_concat ~ty:Clos.ty_string () in
  let args = [fresh_atom_string (); fresh_atom_string (); fresh_atom_string ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_string
    |> Clos.check_expr env expr

let test_check_expr_builtin_invalid_arity _ =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_add ~ty:Clos.ty_int () in
  let expr = fresh_expr_builtin ~fn ~args:[] () in
  let exn = Clos.InvalidArity (2, 0) in
  assert_raises exn (fun _ ->
    Clos.check_expr env expr (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_builtin_fixed_mismatched_types _ =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_add ~ty:Clos.ty_int () in
  let args = [fresh_atom_int (); fresh_atom_bool ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  let exn = Clos.MismatchedTypes (Clos.ty_bool, Clos.ty_int) in
  assert_raises exn (fun _ ->
    Clos.check_expr env expr (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_builtin_var_mismatched_types _ =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_concat ~ty:Clos.ty_string () in
  let args = [fresh_atom_string (); fresh_atom_int (); fresh_atom_string ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  let exn = Clos.MismatchedTypes (Clos.ty_bool, Clos.ty_string) in
  assert_raises exn (fun _ ->
    Clos.check_expr env expr (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let expr =
    let atom = fresh_atom_bool () in
    fresh_expr_atom ~atom ()
  in
  TypeTest.assert_ty_equal ~ctxt Clos.ty_bool
    |> Clos.check_expr env expr

(* Bindings *)

let test_check_binding ctxt =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let ty = Clos.ty_bool in
  let binding =
    let patt = PattTest.fresh_patt_var ~id () in
    let value =
      let atom = fresh_atom_bool () in
      fresh_expr_atom ~atom ()
    in
    fresh_binding ~patt ~ty ~value ()
  in
  Clos.check_binding env binding (fun env ->
    ClosUtils.assert_ty_bound ~ctxt id env ty)

let test_check_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let inferred = Clos.ty_bool in
  let annotated = Clos.ty_int in
  let binding =
    let patt =
      let id = SymTest.fresh_sym () in
      PattTest.fresh_patt_var ~id ()
    in
    let value =
      let atom = fresh_atom_bool () in
      fresh_expr_atom ~atom ()
    in
    Clos.binding patt annotated value
  in
  let exn = Clos.MismatchedTypes (inferred, annotated) in
  assert_raises exn (fun _ ->
    Clos.check_binding env binding (fun _ ->
      assert_failure "Expected exception"))

(* Blocks *)

let test_check_block_let ctxt =
  let env = EnvTest.fresh () in
  let ty = Clos.ty_bool in
  let block =
    let id = SymTest.fresh_sym () in
    let binding =
      let patt = PattTest.fresh_patt_var ~id () in
      let value =
        let atom = fresh_atom_bool () in
        fresh_expr_atom ~atom ()
      in
      fresh_binding ~patt ~ty ~value ()
    in
    let scope =
      let atom = fresh_atom_ident ~id () in
      let expr = fresh_expr_atom ~atom () in
      fresh_block_expr ~expr ()
    in
    fresh_block_let ~binding ~scope ()
  in
  Clos.check_block env block (fun _ inferred ->
    TypeTest.assert_ty_equal ~ctxt ty inferred)

let test_check_block_expr ctxt =
  let env = EnvTest.fresh () in
  let block =
    let atom = fresh_atom_bool () in
    let expr = fresh_expr_atom ~atom () in
    fresh_block_expr ~expr ()
  in
  Clos.check_block env block (fun _ ty ->
    TypeTest.assert_ty_equal ~ctxt Clos.ty_bool ty)

(* Top-Level Expressions *)

let test_check_top_let ctxt =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let ty = Clos.ty_bool in
  let top =
    let binding =
      let patt = PattTest.fresh_patt_var ~id () in
      let value =
        let atom = fresh_atom_bool () in
        fresh_expr_atom ~atom ()
      in
      fresh_binding ~patt ~ty ~value ()
    in
    fresh_top_let ~binding ()
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
      "Built-in Function Application" >::: [
        "Valid"                     >::: [
          "Fixed Arity"    >:: test_check_expr_builtin_fixed;
          "Variable Arity" >:: test_check_expr_builtin_var;
        ];
        "Invalid Arity"             >:: test_check_expr_builtin_invalid_arity;
        "Mismatched Argument Types" >::: [
          "Fixed Arity"    >:: test_check_expr_builtin_fixed_mismatched_types;
          "Variable Arity" >:: test_check_expr_builtin_var_mismatched_types;
        ];
      ];
      "Atoms" >:: test_check_expr_atom;
    ];
    "Bindings" >::: [
      "Valid"            >:: test_check_binding;
      "Mismatched Types" >:: test_check_binding_mismatched_types;
    ];
    "Blocks" >::: [
      "Let Expressions" >:: test_check_block_let;
      "Expressions"     >:: test_check_block_expr;
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
