open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_rune_lit ?loc:(loc = LocTest.gen ()) ?value:(value = 'a') _ =
  value
    |> Uchar.of_char
    |> Syntax.rune_lit loc

let fresh_rune_escape ?loc:(loc = LocTest.gen ()) ?value:(value = 42) _ =
  value
    |> fprintf str_formatter "\\U+%X"
    |> flush_str_formatter
    |> Syntax.rune_escape loc

let fresh_str_lit ?loc:(loc = LocTest.gen ()) ?value:(value = "foo bar") _ =
  Syntax.str_lit loc value

let fresh_str_escape ?loc:(loc = LocTest.gen ()) ?value:(value = 42) _ =
  value
    |> fprintf str_formatter "\\U+%X"
    |> flush_str_formatter
    |> Syntax.str_escape loc

let fresh_expr_bool ?loc:(loc = LocTest.gen ()) ?value:(value = true) _ =
  Syntax.expr_bool loc value

let fresh_expr_int ?loc:(loc = LocTest.gen ()) ?value:(value = 42l) _ =
  value
    |> Int32.to_string
    |> Syntax.expr_int loc

let fresh_expr_long ?loc:(loc = LocTest.gen ()) ?value:(value = 42L) _ =
  value
    |> Int64.to_string
    |> Syntax.expr_long loc

let fresh_expr_float ?loc:(loc = LocTest.gen ()) ?value:(value = 4.2) _ =
  value
    |> sprintf "%g"
    |> Syntax.expr_float loc

let fresh_expr_double ?loc:(loc = LocTest.gen ()) ?value:(value = 4.2) _ =
  value
    |> sprintf "%g"
    |> Syntax.expr_double loc

let fresh_expr_rune ?loc:(loc = LocTest.gen ()) ?value:(value = fresh_rune_lit ()) _ =
  Syntax.expr_rune loc value

let fresh_expr_string ?loc:(loc = LocTest.gen ()) ?value:(value = [fresh_str_lit ()]) _ =
  Syntax.expr_string loc value

let fresh_expr_ident ?loc:(loc = LocTest.gen ()) ?seq:(seq = Sym.seq ()) ?id:(id = "") _ =
  seq
    |> Sym.gen ~id
    |> Syntax.expr_ident loc

let fresh_expr_un_op ?loc:(loc = LocTest.gen ()) ?op:(op = OpTest.fresh_op_un ()) ?operand:(operand = fresh_expr_bool ()) _ =
  Syntax.expr_un_op loc op operand

let fresh_expr_bin_op ?loc:(loc = LocTest.gen ()) ?op:(op = OpTest.fresh_op_bin ()) ?lhs:(lhs = fresh_expr_int ~value:1l ()) ?rhs:(rhs = fresh_expr_int ~value:2l ()) _ =
  Syntax.expr_bin_op loc op lhs rhs

let rec fresh_expr_let ?loc:(loc = LocTest.gen ()) ?binding:(binding = fresh_value_binding ()) ?scope:(scope = fresh_expr_bool ()) _ =
  Syntax.expr_let loc binding scope

and fresh_value_binding ?loc:(loc = LocTest.gen ()) ?patt:(patt = PattTest.fresh_patt_ground ()) ?explicit:(explicit = false) ?ty:(ty = TypeTest.fresh_ty_constr ()) ?value:(value = fresh_expr_bool ()) _ =
  let ty = if explicit then Some ty else None in
  Syntax.value_binding loc patt ty value

let fresh_top_let ?loc:(loc = LocTest.gen ()) ?binding:(binding = fresh_value_binding ()) _ =
  Syntax.top_let loc binding

let fresh_top_val ?loc:(loc = LocTest.gen ()) ?binding:(binding = fresh_value_binding ()) _ =
  Syntax.top_val loc binding

(* Utilities *)

(* Location Stripping *)

let deloc_rune = function
  | Syntax.RuneLit rune -> Syntax.rune_lit LocTest.dummy rune.value
  | Syntax.RuneEscape rune -> Syntax.rune_escape LocTest.dummy rune.lexeme

let deloc_str = function
  | Syntax.StringLit str -> Syntax.str_lit LocTest.dummy str.lexeme
  | Syntax.StringEscape str -> Syntax.str_escape LocTest.dummy str.lexeme

let rec deloc_expr = function
  | Syntax.ExprBool expr -> Syntax.expr_bool LocTest.dummy expr.value
  | Syntax.ExprInt expr -> Syntax.expr_int LocTest.dummy expr.lexeme
  | Syntax.ExprLong expr -> Syntax.expr_long LocTest.dummy expr.lexeme
  | Syntax.ExprFloat expr -> Syntax.expr_float LocTest.dummy expr.lexeme
  | Syntax.ExprDouble expr -> Syntax.expr_double LocTest.dummy expr.lexeme
  | Syntax.ExprRune expr -> deloc_expr_rune expr.value
  | Syntax.ExprString expr -> deloc_expr_string expr.value
  | Syntax.ExprIdent expr -> Syntax.expr_ident LocTest.dummy expr.id
  | Syntax.ExprUnOp expr -> deloc_expr_un_op expr.op expr.operand
  | Syntax.ExprBinOp expr -> deloc_expr_bin_op expr.op expr.lhs expr.rhs
  | Syntax.ExprLet expr -> deloc_expr_let expr.binding expr.scope

and deloc_expr_rune value =
  value
    |> deloc_rune
    |> Syntax.expr_rune LocTest.dummy

and deloc_expr_string value =
  value
    |> List.map deloc_str
    |> Syntax.expr_string LocTest.dummy

and deloc_expr_un_op op operand =
  let op = OpTest.deloc_un op in
  let operand = deloc_expr operand in
  Syntax.expr_un_op LocTest.dummy op operand

and deloc_expr_bin_op op lhs rhs =
  let op = OpTest.deloc_bin op in
  let lhs = deloc_expr lhs in
  let rhs = deloc_expr rhs in
  Syntax.expr_bin_op LocTest.dummy op lhs rhs

and deloc_expr_let binding scope =
  let binding = deloc_binding binding in
  let scope = deloc_expr scope in
  Syntax.expr_let LocTest.dummy binding scope

and deloc_binding = function
  | Syntax.ValueBinding binding ->
    let patt = PattTest.deloc_patt binding.patt in
    let ty = SyntaxUtils.deloc_optional TypeTest.deloc_ty binding.ty in
    binding.value
      |> deloc_expr
      |> Syntax.value_binding LocTest.dummy patt ty

let rec deloc_top = function
  | Syntax.TopLet top -> deloc_top_let top.binding
  | Syntax.TopVal top -> deloc_top_val top.binding

and deloc_top_let binding =
  binding
    |> deloc_binding
    |> Syntax.top_let LocTest.dummy

and deloc_top_val binding =
  binding
    |> deloc_binding
    |> Syntax.top_val LocTest.dummy

(* Assertions *)

let rune_not_equal = TestUtils.not_equal "Runes" Syntax.pp_rune
let str_not_equal = TestUtils.not_equal "Strings" Syntax.pp_str
let expr_not_equal = TestUtils.not_equal "Expressions" Syntax.pp_expr
let top_not_equal = TestUtils.not_equal "Top-level expressions" Syntax.pp_top

let assert_rune_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.RuneLit expected, Syntax.RuneLit actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    let printer r = sprintf "%c" (Uchar.to_char r) in
    assert_equal ~ctxt ~printer ~msg:"Runes are not equal" expected.value actual.value
  | Syntax.RuneEscape expected, Syntax.RuneEscape actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Rune escape sequences are not equal" expected.lexeme actual.lexeme
  | expected, actual -> rune_not_equal ~ctxt expected actual

let assert_str_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.StringLit expected, Syntax.StringLit actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"String literals are not equal" expected.lexeme actual.lexeme
  | Syntax.StringEscape expected, Syntax.StringEscape actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"String escape sequences are not equal" expected.lexeme actual.lexeme
  | expected, actual -> str_not_equal ~ctxt expected actual

let rec assert_expr_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.ExprBool expected, Syntax.ExprBool actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" expected.value actual.value
  | Syntax.ExprInt expected, Syntax.ExprInt actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Integer lexemes are not equal" expected.lexeme actual.lexeme
  | Syntax.ExprLong expected, Syntax.ExprLong actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Long lexemes are not equal" expected.lexeme actual.lexeme
  | Syntax.ExprFloat expected, Syntax.ExprFloat actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Float lexemes are not equal" expected.lexeme actual.lexeme
  | Syntax.ExprDouble expected, Syntax.ExprDouble actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Double lexemes are not equal" expected.lexeme actual.lexeme
  | Syntax.ExprRune expected, Syntax.ExprRune actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_rune_equal ~ctxt expected.value actual.value
  | Syntax.ExprString expected, Syntax.ExprString actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    List.iter2 (assert_str_equal ~ctxt) expected.value actual.value
  | Syntax.ExprIdent expected, Syntax.ExprIdent actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    SymTest.assert_sym_equal ~ctxt expected.id actual.id
  | Syntax.ExprUnOp expected, Syntax.ExprUnOp actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    OpTest.assert_un_equal ~ctxt expected.op actual.op;
    assert_expr_equal ~ctxt expected.operand actual.operand
  | Syntax.ExprBinOp expected, Syntax.ExprBinOp actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    OpTest.assert_bin_equal ~ctxt expected.op actual.op;
    assert_expr_equal ~ctxt expected.lhs actual.lhs;
    assert_expr_equal ~ctxt expected.rhs actual.rhs
  | Syntax.ExprLet expected, Syntax.ExprLet actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_binding_equal ~ctxt expected.binding actual.binding;
    assert_expr_equal ~ctxt expected.scope actual.scope
  | expected, actual -> expr_not_equal ~ctxt expected actual

and assert_binding_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.ValueBinding expected, Syntax.ValueBinding actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    PattTest.assert_patt_equal ~ctxt expected.patt actual.patt;
    TestUtils.assert_optional_equal ~ctxt "type" TypeTest.assert_ty_equal expected.ty actual.ty;
    assert_expr_equal ~ctxt expected.value actual.value

let assert_top_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.TopLet expected, Syntax.TopLet actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_binding_equal ~ctxt expected.binding actual.binding
  | Syntax.TopVal expected, Syntax.TopVal actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc;
    assert_binding_equal ~ctxt expected.binding actual.binding
  | expected, actual -> top_not_equal ~ctxt expected actual

(* Constructors *)

let test_rune_lit ctxt =
  let loc = LocTest.gen () in
  let value = Uchar.of_char 'a' in
  let expected = Syntax.rune_lit loc value in
  match expected with
    | Syntax.RuneLit actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~cmp:Uchar.equal ~printer:Utf8.to_string ~msg:"Rune values are not equal" value actual.value
    | actual -> rune_not_equal ~ctxt expected actual

let test_rune_escape ctxt =
  let loc = LocTest.gen () in
  let value = "\\U+42" in
  let expected = Syntax.rune_escape loc value in
  match expected with
    | Syntax.RuneEscape actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~printer:Fun.id ~msg:"Rune escape lexemes are not equal" value actual.lexeme
    | actual -> rune_not_equal ~ctxt expected actual

let test_str_lit ctxt =
  let loc = LocTest.gen () in
  let value = "foo bar" in
  let expected = Syntax.str_lit loc value in
  match expected with
    | Syntax.StringLit actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~printer:Fun.id ~msg:"String literal lexemes are not equal" value actual.lexeme
    | actual -> str_not_equal ~ctxt expected actual

let test_str_escape ctxt =
  let loc = LocTest.gen () in
  let value = "\\U+42" in
  let expected = Syntax.str_escape loc value in
  match expected with
    | Syntax.StringEscape actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~printer:Fun.id ~msg:"String escape lexemes are not equal" value actual.lexeme
    | actual -> str_not_equal ~ctxt expected actual

let test_expr_bool ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.expr_bool loc true in
  match expected with
    | Syntax.ExprBool actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Boolean values are not equal" ~printer:string_of_bool true actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_int ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+42i" in
  let expected = Syntax.expr_int loc lexeme in
  match expected with
    | Syntax.ExprInt actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Int lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_long ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+42L" in
  let expected = Syntax.expr_long loc lexeme in
  match expected with
    | Syntax.ExprLong actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Long lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_float ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+1.2e-3.4f" in
  let expected = Syntax.expr_float loc lexeme in
  match expected with
    | Syntax.ExprFloat actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Float lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_double ctxt =
  let loc = LocTest.gen () in
  let lexeme = "+1.2e-3.4D" in
  let expected = Syntax.expr_double loc lexeme in
  match expected with
    | Syntax.ExprDouble actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_equal ~ctxt ~msg:"Double lexemes are not equal" ~printer:Fun.id lexeme actual.lexeme
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_rune ctxt =
  let value = fresh_rune_lit () in
  let loc = LocTest.gen () in
  let expected = Syntax.expr_rune loc value in
  match expected with
    | Syntax.ExprRune actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_rune_equal ~ctxt value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_string ctxt =
  let value = [fresh_str_lit ()] in
  let loc = LocTest.gen () in
  let expected = Syntax.expr_string loc value in
  match expected with
    | Syntax.ExprString actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      List.iter2 (assert_str_equal ~ctxt) value actual.value
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_ident ctxt =
  let loc = LocTest.gen () in
  let id = () |> Sym.seq |> Sym.gen in
  let expected = Syntax.expr_ident loc id in
  match expected with
    | Syntax.ExprIdent actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      SymTest.assert_sym_equal ~ctxt id actual.id
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_un_op ctxt =
  let loc = LocTest.gen () in
  let op = OpTest.fresh_op_un () in
  let operand = fresh_expr_int () in
  let expected = Syntax.expr_un_op loc op operand in
  match expected with
    | Syntax.ExprUnOp actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      OpTest.assert_un_equal ~ctxt op actual.op;
      assert_expr_equal ~ctxt operand actual.operand
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_bin_op ctxt =
  let loc = LocTest.gen () in
  let op = OpTest.fresh_op_bin () in
  let lhs = fresh_expr_int ~value:1l () in
  let rhs = fresh_expr_int ~value:2l () in
  let expected = Syntax.expr_bin_op loc op lhs rhs in
  match expected with
    | Syntax.ExprBinOp actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      OpTest.assert_bin_equal ~ctxt op actual.op;
      assert_expr_equal ~ctxt lhs actual.lhs;
      assert_expr_equal ~ctxt rhs actual.rhs
    | actual -> expr_not_equal ~ctxt expected actual

let test_expr_let ctxt =
  let loc = LocTest.gen () in
  let binding = fresh_value_binding () in
  let scope = fresh_expr_bool () in
  let expected = Syntax.expr_let loc binding scope in
  match expected with
    | Syntax.ExprLet actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_binding_equal ~ctxt binding actual.binding;
      assert_expr_equal ~ctxt scope actual.scope
    | actual -> expr_not_equal ~ctxt expected actual

let test_binding_value_binding ctxt =
  let patt = PattTest.fresh_patt_ground () in
  let value = fresh_expr_bool () in
  let assert_binding_equal ty =
    let loc = LocTest.gen () in
    let expected = Syntax.value_binding loc patt ty value in
    match expected with
      | Syntax.ValueBinding actual ->
        LocTest.assert_loc_equal ~ctxt loc actual.loc;
        PattTest.assert_patt_equal ~ctxt patt actual.patt;
        TestUtils.assert_optional_equal ~ctxt "Type" TypeTest.assert_ty_equal ty actual.ty;
        assert_expr_equal ~ctxt value actual.value
  in
  assert_binding_equal None;
  assert_binding_equal (Some (TypeTest.fresh_ty_constr ()))

let test_top_let ctxt =
  let loc = LocTest.gen () in
  let binding = fresh_value_binding () in
  let expected = Syntax.top_let loc binding in
  match expected with
    | Syntax.TopLet actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_binding_equal ~ctxt binding actual.binding
    | actual -> top_not_equal ~ctxt expected actual

let test_top_val ctxt =
  let loc = LocTest.gen () in
  let binding = fresh_value_binding () in
  let expected = Syntax.top_val loc binding in
  match expected with
    | Syntax.TopVal actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc;
      assert_binding_equal ~ctxt binding actual.binding
    | actual -> top_not_equal ~ctxt expected actual

let test_constructor =
  "Constructors" >::: [
    "Runes" >::: [
      "Literals"                 >:: test_rune_lit;
      "Unicode Escape Sequences" >:: test_rune_escape;
    ];
    "Strings" >::: [
      "Literals"                 >:: test_str_lit;
      "Unicode Escape Sequences" >:: test_str_escape;
    ];
    "Expressions" >::: [
      "Booleans"         >:: test_expr_bool;
      "Integers"         >:: test_expr_int;
      "Longs"            >:: test_expr_long;
      "Floats"           >:: test_expr_float;
      "Doubles"          >:: test_expr_double;
      "Runes"            >:: test_expr_rune;
      "Strings"          >:: test_expr_string;
      "Identifiers"      >:: test_expr_ident;
      "Unary Operators"  >:: test_expr_un_op;
      "Binary Operators" >:: test_expr_bin_op;
      "Let Bindings"     >:: test_expr_let;
    ];
    "Bindings" >:: test_binding_value_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_top_let;
      "Val Bindings" >:: test_top_val;
    ];
  ]

(* Locations *)

let assert_loc_rune = SyntaxUtils.assert_loc Syntax.loc_rune
let assert_loc_str = SyntaxUtils.assert_loc Syntax.loc_str
let assert_loc_expr = SyntaxUtils.assert_loc Syntax.loc_expr
let assert_loc_binding = SyntaxUtils.assert_loc Syntax.loc_binding
let assert_loc_top = SyntaxUtils.assert_loc Syntax.loc_top

let test_loc_rune_lit = assert_loc_rune (fun loc -> fresh_rune_lit ~loc ())
let test_loc_rune_escape = assert_loc_rune (fun loc -> fresh_rune_escape ~loc ())

let test_loc_str_lit = assert_loc_str (fun loc -> fresh_str_lit ~loc ())
let test_loc_str_escape = assert_loc_str (fun loc -> fresh_str_escape ~loc ())

let test_loc_expr_bool = assert_loc_expr (fun loc -> fresh_expr_bool ~loc ())
let test_loc_expr_int = assert_loc_expr (fun loc -> fresh_expr_int ~loc ())
let test_loc_expr_long = assert_loc_expr (fun loc -> fresh_expr_long ~loc ())
let test_loc_expr_float = assert_loc_expr (fun loc -> fresh_expr_float ~loc ())
let test_loc_expr_double = assert_loc_expr (fun loc -> fresh_expr_double ~loc ())
let test_loc_expr_rune = assert_loc_expr (fun loc -> fresh_expr_rune ~loc ())
let test_loc_expr_string = assert_loc_expr (fun loc -> fresh_expr_string ~loc ())
let test_loc_expr_ident = assert_loc_expr (fun loc -> fresh_expr_ident ~loc ())
let test_loc_expr_un_op = assert_loc_expr (fun loc -> fresh_expr_un_op ~loc ())
let test_loc_expr_bin_op = assert_loc_expr (fun loc -> fresh_expr_bin_op ~loc ())
let test_loc_expr_let = assert_loc_expr (fun loc -> fresh_expr_let ~loc ())

let test_loc_binding_value_binding = assert_loc_binding (fun loc -> fresh_value_binding ~loc ())

let test_loc_top_let = assert_loc_top (fun loc -> fresh_top_let ~loc ())
let test_loc_top_val = assert_loc_top (fun loc -> fresh_top_val ~loc ())

let test_loc =
  "Locations" >::: [
    "Runes" >::: [
      "Literals"                 >:: test_loc_rune_lit;
      "Unicode Escape Sequences" >:: test_loc_rune_escape;
    ];
    "Strings" >::: [
      "Literals"                 >:: test_loc_str_lit;
      "Unicode Escape Sequences" >:: test_loc_str_escape;
    ];
    "Expressions" >::: [
      "Booleans"         >:: test_loc_expr_bool;
      "Integers"         >:: test_loc_expr_int;
      "Longs"            >:: test_loc_expr_long;
      "Floats"           >:: test_loc_expr_float;
      "Doubles"          >:: test_loc_expr_double;
      "Runes"            >:: test_loc_expr_rune;
      "Strings"          >:: test_loc_expr_string;
      "Identifiers"      >:: test_loc_expr_ident;
      "Unary Operators"  >:: test_loc_expr_un_op;
      "Binary Operators" >:: test_loc_expr_bin_op;
      "Let Bindings"     >:: test_loc_expr_let;
    ];
    "Bindings" >:: test_loc_binding_value_binding;
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_loc_top_let;
      "Val Bindings" >:: test_loc_top_val;
    ];
  ]

(* Pretty Printing *)

let assert_pp_rune = PrettyTest.assert_pp Syntax.pp_rune
let assert_pp_str = PrettyTest.assert_pp Syntax.pp_str
let assert_pp_expr = PrettyTest.assert_pp Syntax.pp_expr
let assert_pp_binding = PrettyTest.assert_pp Syntax.pp_binding
let assert_pp_top = PrettyTest.assert_pp Syntax.pp_top

let test_pp_expr_bool ctxt =
  Syntax.expr_bool LocTest.dummy true
    |> assert_pp_expr ~ctxt ["true"];
  Syntax.expr_bool LocTest.dummy false
    |> assert_pp_expr ~ctxt ["false"]

let test_pp_expr_int ctxt =
  Syntax.expr_int LocTest.dummy "+42i"
    |> assert_pp_expr ~ctxt ["+42i"]

let test_pp_expr_long ctxt =
  Syntax.expr_long LocTest.dummy "+42L"
    |> assert_pp_expr ~ctxt ["+42L"]

let test_pp_expr_float ctxt =
  Syntax.expr_float LocTest.dummy "+1.2e-3.4f"
    |> assert_pp_expr ~ctxt ["+1.2e-3.4f"]

let test_pp_expr_double ctxt =
  Syntax.expr_double LocTest.dummy "+1.2e-3.4D"
    |> assert_pp_expr ~ctxt ["+1.2e-3.4D"]

let test_pp_expr_rune ctxt =
  let rune = 'a' in
  let value = fresh_rune_lit ~value:rune () in
  fresh_expr_rune ~value ()
    |> assert_pp_expr ~ctxt [
         sprintf "'%c'" rune
       ]

let test_pp_expr_string ctxt =
  let lexeme = "asdf" in
  let value = [fresh_str_lit ~value:lexeme ()] in
  fresh_expr_string ~value ()
    |> assert_pp_expr ~ctxt [
         sprintf "%S" lexeme
       ]

let test_pp_binding_value_binding ctxt =
  let id = "testId" in

  let patt = PattTest.fresh_patt_var ~id () in
  let ty = TypeTest.fresh_ty_constr ~id:Prim.id_bool () in
  let value = fresh_expr_bool ~value:true () in

  fresh_value_binding ~patt ~explicit:true ~ty ~value ()
    |> assert_pp_binding ~ctxt [
         sprintf "%s: %s = %B" id Prim.id_bool true
       ];
  fresh_value_binding ~patt ~value ()
    |> assert_pp_binding ~ctxt [
         sprintf "%s = %B" id true
       ]

let test_pp_top_let ctxt =
  let id = "testId" in

  let patt = PattTest.fresh_patt_var ~id () in
  let value = fresh_expr_bool ~value:true () in
  let binding = fresh_value_binding ~patt ~value () in

  fresh_top_let ~binding ()
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "let %a" Syntax.pp_binding binding |> flush_str_formatter
       ]

let test_pp_top_val ctxt =
  let id = "testId" in

  let patt = PattTest.fresh_patt_var ~id () in
  let value = fresh_expr_bool ~value:true () in
  let binding = fresh_value_binding ~patt ~value () in

  fresh_top_val ~binding ()
    |> assert_pp_top ~ctxt [
         fprintf str_formatter "val %a" Syntax.pp_binding binding |> flush_str_formatter
       ]

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
    "Bindings" >::: [
      "Value Bindings" >:: test_pp_binding_value_binding;
    ];
    "Top-Level Expressions" >::: [
      "Let Bindings" >:: test_pp_top_let;
      "Val Bindings" >:: test_pp_top_val;
    ];
  ]

(* Test Suite *)

let suite =
  "Abstract Syntax" >::: [
    test_constructor;
    test_loc;
    test_pp;
  ]
