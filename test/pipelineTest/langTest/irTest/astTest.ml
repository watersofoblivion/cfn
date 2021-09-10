(* Abstract Syntax *)

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

let fresh_expr_builtin ?fn:(fn = BuiltinTest.fresh_builtin_add ()) ?args:(args = [fresh_atom_int (); fresh_atom_int ()]) _ =
  Ir.expr_builtin fn args

let fresh_expr_atom ?atom:(atom = fresh_atom_bool ()) _ =
  Ir.expr_atom atom

let fresh_binding ?patt:(patt = PattTest.fresh_patt_ground ()) ?ty:(ty = Ir.ty_bool) ?value:(value = fresh_expr_atom ()) _ =
  Ir.binding patt ty value

let fresh_term_expr ?expr:(expr = fresh_expr_atom ()) _ =
  Ir.term_expr expr

let fresh_term_let ?binding:(binding = fresh_binding ()) ?scope:(scope = fresh_term_expr ()) _ =
  Ir.term_let binding scope

let fresh_top_let ?binding:(binding = fresh_binding ()) _ =
  Ir.top_let binding

(* Assertions *)

let atom_not_equal = TestUtils.not_equal "Atomic values" Ir.pp_atom
let expr_not_equal = TestUtils.not_equal "Expressions" Ir.pp_expr
(* let binding_not_equal = TestUtils.not_equal "Bindings" Ir.pp_binding *)
let term_not_equal = TestUtils.not_equal "Term values" Ir.pp_term
(* let top_not_equal = TestUtils.not_equal "Top-level expressions" Ir.pp_top *)

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

let assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.ExprBuiltin expected, Ir.ExprBuiltin actual ->
    BuiltinTest.assert_builtin_equal ~ctxt expected.fn actual.fn;
    List.iter2 (assert_atom_equal ~ctxt) expected.args actual.args
  | Ir.ExprAtom expected, Ir.ExprAtom actual ->
    assert_atom_equal ~ctxt expected.atom actual.atom
  | expected, actual -> expr_not_equal ~ctxt expected actual

let assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.Binding expected, Ir.Binding actual ->
    PattTest.assert_patt_equal ~ctxt expected.patt actual.patt;
    TypeTest.assert_ty_equal ~ctxt expected.ty actual.ty;
    assert_expr_equal ~ctxt expected.value actual.value

let rec assert_term_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.TermLet expected, Ir.TermLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding;
    assert_term_equal ~ctxt expected.scope actual.scope
  | Ir.TermExpr expected, Ir.TermExpr actual ->
    assert_expr_equal ~ctxt expected.expr actual.expr
  | expected, actual -> term_not_equal ~ctxt expected actual

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Ir.TopLet expected, Ir.TopLet actual ->
    assert_binding_equal ~ctxt expected.binding actual.binding

(* Tests *)

(* Constructors *)

(* Atoms *)

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

(* Expressions *)

let test_expr_builtin ctxt =
  let fn = BuiltinTest.fresh_builtin_add () in
  let args = [fresh_atom_int (); fresh_atom_long ()] in
  let expected = Ir.expr_builtin fn args in
  match expected with
    | Ir.ExprBuiltin actual ->
      BuiltinTest.assert_builtin_equal ~ctxt fn actual.fn;
      List.iter2 (assert_atom_equal ~ctxt) args actual.args
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_atom ctxt =
  let atom = fresh_atom_bool () in
  let expected = Ir.expr_atom atom in
  match expected with
    | Ir.ExprAtom actual ->
      assert_atom_equal ~ctxt atom actual.atom
    | actual -> expr_not_equal ~ctxt expected actual

(* Terms *)

let test_term_let ctxt =
  let binding = fresh_binding () in
  let scope = fresh_term_expr () in
  let expected = Ir.term_let binding scope in
  match expected with
    | Ir.TermLet actual ->
      assert_binding_equal ~ctxt binding actual.binding;
      assert_term_equal ~ctxt scope actual.scope
    | actual -> term_not_equal ~ctxt expected actual

let test_term_expr ctxt =
  let expr = fresh_expr_atom () in
  let expected = Ir.term_expr expr in
  match expected with
    | Ir.TermExpr actual ->
      assert_expr_equal ~ctxt expr actual.expr
    | actual -> term_not_equal ~ctxt expected actual

(* Bindings *)

let test_binding ctxt =
  let patt = PattTest.fresh_patt_ground () in
  let ty = Ir.ty_bool in
  let value = fresh_expr_atom () in
  let expected = Ir.binding patt ty value in
  match expected with
    | Ir.Binding actual ->
      PattTest.assert_patt_equal ~ctxt patt actual.patt;
      TypeTest.assert_ty_equal ~ctxt ty actual.ty;
      assert_expr_equal ~ctxt value actual.value

(* Top-Level Expressions *)

let test_top_let ctxt =
  let binding = fresh_binding () in
  let expected = Ir.top_let binding in
  match expected with
    | Ir.TopLet actual ->
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
    "Terms" >::: [
      "Let Bindings" >:: test_term_let;
      "Expressions"  >:: test_term_expr;
    ];
    "Bindings" >:: test_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_top_let;
    ];
  ]

(* Pretty Printing *)

let assert_pp_atom = PrettyTest.assert_pp Ir.pp_atom
let assert_pp_expr = PrettyTest.assert_pp Ir.pp_expr
let assert_pp_term = PrettyTest.assert_pp Ir.pp_term
let assert_pp_binding = PrettyTest.assert_pp Ir.pp_binding
let assert_pp_top = PrettyTest.assert_pp Ir.pp_top

(* Atoms *)

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

(* Expressions *)

let test_pp_expr_builtin ctxt =
  let fn = BuiltinTest.fresh_builtin_struct_eq () in
  let args = [
    fresh_atom_bool ();
    fresh_atom_int ();
  ] in
  let pp_sep fmt _ = fprintf fmt " " in
  fresh_expr_builtin ~fn ~args ()
    |> assert_pp_expr ~ctxt [
         fprintf str_formatter "%a %a" Ir.pp_builtin fn (pp_print_list ~pp_sep Ir.pp_atom) args
           |> flush_str_formatter;
       ]

let test_pp_expr_atom ctxt =
  let atom = fresh_atom_bool ~value:true () in
  fresh_expr_atom ~atom ()
    |> assert_pp_expr ~ctxt ["true"]

(* Bindings *)

let test_pp_binding ctxt =
  let patt = PattTest.fresh_patt_ground () in
  let ty = Ir.ty_bool in
  let value =
    let atom = fresh_atom_bool ~value:true () in
    fresh_expr_atom ~atom ()
  in
  fresh_binding ~patt ~ty ~value ()
    |> assert_pp_binding ~ctxt [
         fprintf str_formatter "%a: %a = %a" Ir.pp_patt patt Ir.pp_ty ty Ir.pp_expr value
           |> flush_str_formatter
       ]

(* Terms *)

let test_pp_term_let ctxt =
  let binding = fresh_binding () in
  let scope = fresh_term_expr () in
  fresh_term_let ~binding ~scope ()
    |> assert_pp_term ~ctxt [
         fprintf str_formatter "let %a in %a" Ir.pp_binding binding Ir.pp_term scope
           |> flush_str_formatter
       ]

let test_pp_term_expr ctxt =
  let atom = fresh_atom_bool ~value:true () in
  let expr = fresh_expr_atom ~atom () in
  fresh_term_expr ~expr ()
    |> assert_pp_term ~ctxt ["true"]

(* Top-Level Expressions *)

let test_pp_top_let ctxt =
  let binding = fresh_binding () in
  Ir.top_let binding
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Ir.pp_binding binding |> flush_str_formatter
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
    "Terms" >::: [
      "Let Bindings" >:: test_pp_term_let;
      "Expressions"  >:: test_pp_term_expr;
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
  let exn = Ir.UnboundIdentifier id in
  assert_raises exn (fun _ ->
    Ir.check_atom env atom (fun _ ->
      assert_failure "Expected exception"))

(* Expressions *)

let test_check_expr_builtin_fixed ctxt =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_struct_eq ~ty:Ir.ty_int () in
  let args = [fresh_atom_int (); fresh_atom_int ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_bool
    |> Ir.check_expr env expr

let test_check_expr_builtin_var ctxt =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_concat ~ty:Ir.ty_string () in
  let args = [fresh_atom_string (); fresh_atom_string (); fresh_atom_string ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_string
    |> Ir.check_expr env expr

let test_check_expr_builtin_invalid_arity _ =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_add ~ty:Ir.ty_int () in
  let expr = fresh_expr_builtin ~fn ~args:[] () in
  let exn = Ir.InvalidArity (2, 0) in
  assert_raises exn (fun _ ->
    Ir.check_expr env expr (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_builtin_fixed_mismatched_types _ =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_add ~ty:Ir.ty_int () in
  let args = [fresh_atom_int (); fresh_atom_bool ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  let exn = Ir.MismatchedTypes (Ir.ty_bool, Ir.ty_int) in
  assert_raises exn (fun _ ->
    Ir.check_expr env expr (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_builtin_var_mismatched_types _ =
  let env = EnvTest.fresh () in
  let fn = BuiltinTest.fresh_builtin_concat ~ty:Ir.ty_string () in
  let args = [fresh_atom_string (); fresh_atom_int (); fresh_atom_string ()] in
  let expr = fresh_expr_builtin ~fn ~args () in
  let exn = Ir.MismatchedTypes (Ir.ty_int, Ir.ty_string) in
  assert_raises exn (fun _ ->
    Ir.check_expr env expr (fun _ ->
      assert_failure "Expected exception"))

let test_check_expr_atom ctxt =
  let env = EnvTest.fresh () in
  let expr =
    let atom = fresh_atom_bool () in
    fresh_expr_atom ~atom ()
  in
  TypeTest.assert_ty_equal ~ctxt Ir.ty_bool
    |> Ir.check_expr env expr

(* Bindings *)

let test_check_binding ctxt =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let ty = Ir.ty_bool in
  let binding =
    let patt = PattTest.fresh_patt_var ~id () in
    let value =
      let atom = fresh_atom_bool () in
      fresh_expr_atom ~atom ()
    in
    fresh_binding ~patt ~ty ~value ()
  in
  Ir.check_binding env binding (fun env ->
    IrUtils.assert_ty_bound ~ctxt id env ty)

let test_check_binding_mismatched_types _ =
  let env = EnvTest.fresh () in
  let inferred = Ir.ty_bool in
  let annotated = Ir.ty_int in
  let binding =
    let patt =
      let id = SymTest.fresh_sym () in
      PattTest.fresh_patt_var ~id ()
    in
    let value =
      let atom = fresh_atom_bool () in
      fresh_expr_atom ~atom ()
    in
    Ir.binding patt annotated value
  in
  let exn = Ir.MismatchedTypes (inferred, annotated) in
  assert_raises exn (fun _ ->
    Ir.check_binding env binding (fun _ ->
      assert_failure "Expected exception"))

(* Terms *)

let test_check_term_let ctxt =
  let env = EnvTest.fresh () in
  let ty = Ir.ty_bool in
  let term =
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
      fresh_term_expr ~expr ()
    in
    fresh_term_let ~binding ~scope ()
  in
  Ir.check_term env term (fun _ inferred ->
    TypeTest.assert_ty_equal ~ctxt ty inferred)

let test_check_term_expr ctxt =
  let env = EnvTest.fresh () in
  let term =
    let atom = fresh_atom_bool () in
    let expr = fresh_expr_atom ~atom () in
    fresh_term_expr ~expr ()
  in
  Ir.check_term env term (fun _ ty ->
    TypeTest.assert_ty_equal ~ctxt Ir.ty_bool ty)

(* Top-Level Expressions *)

let test_check_top_let ctxt =
  let env = EnvTest.fresh () in
  let id = SymTest.fresh_sym () in
  let ty = Ir.ty_bool in
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
  Ir.check_top env top (fun env ->
    IrUtils.assert_ty_bound ~ctxt id env ty)

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
    "Terms" >::: [
      "Let Expressions" >:: test_check_term_let;
      "Expressions"     >:: test_check_term_expr;
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
