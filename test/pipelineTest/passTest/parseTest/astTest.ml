(* Abstract Syntax *)

open Format

open OUnit2

open Common

open CommonTest

(* Fixtures *)

let fresh_rune_lit ?start:(start = ParseUtils.bof) ?value:(value = 'a') _ =
  let loc =
    value
      |> Uchar.of_char
      |> Utf8.to_string
      |> ParseUtils.lexeme_loc start
  in
  SyntaxTest.fresh_rune_lit ~loc ~value ()

let fresh_atom_bool ?start:(start = ParseUtils.bof) ?value:(value = true) _ =
  let loc =
    let len = if value then 4 else 5 in
    ParseUtils.loc_of_len start len len
  in
  SyntaxTest.fresh_expr_bool ~loc ~value ()

let fresh_atom_int ?start:(start = ParseUtils.bof) ?lexeme:(lexeme = "42") _ =
  let loc = ParseUtils.lexeme_loc start lexeme in
  Syntax.expr_int loc lexeme

let fresh_atom_long ?start:(start = ParseUtils.bof) ?lexeme:(lexeme = "42") _ =
  let loc = ParseUtils.lexeme_loc start lexeme in
  Syntax.expr_long loc lexeme

let fresh_atom_float ?start:(start = ParseUtils.bof) ?lexeme:(lexeme = "4.2") _ =
  let loc = ParseUtils.lexeme_loc start lexeme in
  Syntax.expr_float loc lexeme

let fresh_atom_double ?start:(start = ParseUtils.bof) ?lexeme:(lexeme = "4.2") _ =
  let loc = ParseUtils.lexeme_loc start lexeme in
  Syntax.expr_double loc lexeme

let fresh_atom_ident ?start:(start = ParseUtils.bof) ?id:(id = SymTest.fresh_sym ()) _ =
  let loc = ParseUtils.sym_loc start id in
  Syntax.expr_ident loc id

let fresh_un_op ?op:(op = SyntaxTest.fresh_un_neg ()) ?operand:(operand = fresh_atom_int ()) _ =
  let from = Syntax.loc_un op in
  let loc =
    operand
      |> Syntax.loc_expr
      |> LocTest.span from
  in
  SyntaxTest.fresh_expr_un_op ~loc ~op ~operand ()

let fresh_bin_op ?op:(op = SyntaxTest.fresh_bin_struct_eq ()) ?lhs:(lhs = fresh_atom_bool ()) ?rhs:(rhs = fresh_atom_bool ()) _ =
  let from = Syntax.loc_expr lhs in
  let loc =
    rhs
      |> Syntax.loc_expr
      |> LocTest.span from
  in
  SyntaxTest.fresh_expr_bin_op ~loc ~op ~lhs ~rhs ()

let fresh_value_binding ?patt:(patt = PattTest.fresh_patt_ground ()) ?explicit:(explicit = false) ?ty:(ty = TypeTest.fresh_ty_constr ()) ?value:(value = fresh_atom_bool ()) _ =
  let from = Syntax.loc_patt patt in
  let loc =
    value
      |> Syntax.loc_expr
      |> LocTest.span from
  in
  SyntaxTest.fresh_value_binding ~loc ~patt ~explicit ~ty ~value ()

let fresh_block_let ?start:(start = ParseUtils.bof) ?binding:(binding = fresh_value_binding ()) ?scope:(scope = fresh_atom_bool ()) _ =
  let loc =
    scope
      |> Syntax.loc_expr
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_expr_let ~loc ~binding ~scope ()

(* Generators *)

let signs = [""; "+"; "-"]

let integral_examples = [
  ([""], ["42"]);
  (["0b"; "0B"], ["1001"]);
  (["0o"; "0O"], ["52"]);
  (["0x"; "0X"], ["2a"; "2A"]);
]

let gen_integral_lexemes suffixes =
  List.fold_left (fun acc suffix ->
    List.fold_left (fun acc (radixes, examples) ->
      List.fold_left (fun acc radix ->
        List.fold_left (fun acc example ->
          List.fold_left (fun acc sign ->
            sprintf "%s%s%s%s" sign radix example suffix :: acc
          ) acc signs
        ) acc examples
      ) acc radixes
    ) acc integral_examples
  ) [] suffixes

let gen_floating_point_lexemes suffixes =
  List.fold_left (fun acc suffix ->
    List.fold_left (fun acc msign ->
      (sprintf "%s4.2%s" msign suffix) ::
      List.fold_left (fun acc esign ->
        sprintf "%s4.2e%s42%s" msign esign suffix ::
        sprintf "%s4.2E%s42%s" msign esign suffix ::
        acc
      ) acc signs
    ) acc signs
  ) [] suffixes

let int_lexemes = gen_integral_lexemes [""; "i"; "I"]
let long_lexemes = gen_integral_lexemes ["l"; "L"]
let float_lexemes = gen_floating_point_lexemes [""; "f"; "F"]
let double_lexemes = gen_floating_point_lexemes ["d"; "D"]

(* Assertions *)

let assert_parses_rune = ParseUtils.assert_parses Parse.parse_rune SyntaxTest.assert_rune_equal
let assert_parses_str = ParseUtils.assert_parses Parse.parse_str SyntaxTest.assert_str_equal
let assert_parses_lit = ParseUtils.assert_parses Parse.parse_lit SyntaxTest.assert_expr_equal
let assert_parses_ident = ParseUtils.assert_parses Parse.parse_ident SyntaxTest.assert_expr_equal
let assert_parses_atom = ParseUtils.assert_parses Parse.parse_atom SyntaxTest.assert_expr_equal
let assert_parses_expr = ParseUtils.assert_parses Parse.parse_expr SyntaxTest.assert_expr_equal
let assert_parses_binding = ParseUtils.assert_parses Parse.parse_binding SyntaxTest.assert_binding_equal
let assert_parses_block = ParseUtils.assert_parses Parse.parse_block SyntaxTest.assert_expr_equal

let assert_parses_rune_lit value input ctxt =
  fresh_rune_lit ~value ()
    |> assert_parses_rune ~ctxt [input]

(* Tests *)

(* Runes *)

(* let test_parse_rune_lit = assert_parses_rune_lit 'a' "a" *)
(* let test_parse_rune_lit_esc_squote = assert_parses_rune_lit '\'' "\\'" *)
(* let test_parse_rune_lit_cr = assert_parses_rune_lit '\r' "\\r" *)
(* let test_parse_rune_lit_lf = assert_parses_rune_lit '\n' "\\n" *)
(* let test_parse_rune_lit_tab = assert_parses_rune_lit '\t' "\\t" *)

(* Strings *)

(* Literals *)

let test_parse_lit_bool ctxt =
  fresh_atom_bool ~value:true ()
    |> assert_parses_lit ~ctxt ["true"];
  fresh_atom_bool ~value:false ()
    |> assert_parses_lit ~ctxt ["false"]

let test_parse_lit_int ctxt =
  let assert_parse_expr_int lexeme =
    fresh_atom_int ~lexeme ()
      |> assert_parses_lit ~ctxt [lexeme]
  in
  List.iter assert_parse_expr_int int_lexemes

let test_parse_lit_long ctxt =
  let assert_parse_expr_long lexeme =
    fresh_atom_long ~lexeme ()
      |> assert_parses_lit ~ctxt [lexeme]
  in
  List.iter assert_parse_expr_long long_lexemes

let test_parse_lit_float ctxt =
  let assert_parse_expr_float lexeme =
    fresh_atom_float ~lexeme ()
      |> assert_parses_lit ~ctxt [lexeme]
  in
  List.iter assert_parse_expr_float float_lexemes

let test_parse_lit_double ctxt =
  let assert_parse_expr_double lexeme =
    fresh_atom_double ~lexeme ()
      |> assert_parses_lit ~ctxt [lexeme]
  in
  List.iter assert_parse_expr_double double_lexemes

let test_parse_lit_rune _ = ()
let test_parse_lit_string _ = ()

(* Identifiers *)

let test_parse_ident_lident ctxt =
  let lexeme = "lowerCamelCaseIdentifier" in
  let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
  fresh_atom_ident ~id ()
    |> assert_parses_ident ~ctxt [lexeme]

let test_parse_ident_uident ctxt =
  let lexeme = "UpperCamelCaseIdentifier" in
  let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
  fresh_atom_ident ~id ()
    |> assert_parses_ident ~ctxt [lexeme]

(* Atoms *)

let test_parse_atom_lit ctxt =
  fresh_atom_bool ~value:true ()
    |> assert_parses_atom ~ctxt ["true"]

let test_parse_atom_ident ctxt =
  let lexeme = "lowerCamelCaseIdentifier" in
  let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
  fresh_atom_ident ~id ()
    |> assert_parses_atom ~ctxt [lexeme]

let test_parse_atom_paren ctxt =
  let seq = Sym.seq () in
  let lexeme = "testId" in
  let id = Sym.gen seq ~id:lexeme in
  let binding =
    let patt = PattTest.fresh_patt_var ~start:(0, 5, 5) ~id () in
    let ty =
      let id = Sym.gen seq ~id:Prim.id_bool in
      TypeTest.fresh_ty_constr ~start:(0, 13, 13) ~id ()
    in
    let value = fresh_atom_bool ~start:(0, 20, 20) ~value:true () in
    fresh_value_binding ~patt ~explicit:true ~ty ~value ()
  in
  let scope = fresh_atom_ident ~start:(0, 28, 28) ~id () in
  fresh_block_let ~start:(0, 1, 1) ~binding ~scope ()
    |> assert_parses_atom ~ctxt [
         sprintf "(let %s: %s = true in %s)" lexeme Prim.id_bool lexeme
       ]

(* Expressions *)

type assoc = Left | Right

let un_ops = [
  (Syntax.un_neg,     "-");
  (Syntax.un_log_not, "!");
  (Syntax.un_bit_not, "~");
]

let bin_ops = [
  [(Right, Syntax.bin_exp, "^^")];
  [
    (Left, Syntax.bin_mul, "*");
    (Left, Syntax.bin_div, "/");
    (Left, Syntax.bin_mod, "%")
  ];
  [
    (Left, Syntax.bin_add, "+");
    (Left, Syntax.bin_sub, "-")
  ];
  [
    (Left, Syntax.bin_lt,  "<");
    (Left, Syntax.bin_lte, "<=");
    (Left, Syntax.bin_gt,  ">");
    (Left, Syntax.bin_gte, ">=")
  ];
  [(Left, Syntax.bin_bit_and, "&")];
  [(Left, Syntax.bin_bit_xor, "^")];
  [(Left, Syntax.bin_bit_or,  "|")];
  [
    (Left, Syntax.bin_struct_eq,  "==");
    (Left, Syntax.bin_struct_neq, "!=");
    (Left, Syntax.bin_phys_eq,    "===");
    (Left, Syntax.bin_phys_neq,   "!==")
  ];
  [(Left, Syntax.bin_log_and, "&&")];
  [(Left, Syntax.bin_log_or,  "||")];
]

let test_parse_expr_un_op ctxt =
  let rec not_self acc = function
    | [] -> ()
    | constr :: tl ->
      List.iter () acc;
      List.iter () tl;
  in
  not_self [] un_ops

let test_parse_expr_bin_op ctxt =
  let rec not_self acc = function
    | [] -> ()
    | (assoc, constr) :: tl ->
      let _ = match assoc with
        | Left ->
        | Right ->
      in
      List.iter () acc;
      List.iter () tl;
  in
  not_self [] bin_ops

let test_parse_expr_atom ctxt =
  fresh_atom_bool ~value:true ()
    |> assert_parses_expr ~ctxt ["true"]

(* Bindings *)

let test_parse_binding_value_binding_implicit ctxt =
  let lexeme = "testId" in
  let patt =
    let id = () |> Sym.seq |> Sym.gen ~id:lexeme in
    PattTest.fresh_patt_var ~id ()
  in
  let value = fresh_atom_bool ~start:(0, 9, 9) ~value:true () in
  fresh_value_binding ~patt ~explicit:false ~value ()
    |> assert_parses_binding ~ctxt [
         sprintf "%s = true" lexeme
       ]

let test_parse_binding_value_binding_explicit ctxt =
  let seq = Sym.seq () in
  let lexeme = "testId" in
  let patt =
    let id = Sym.gen seq ~id:lexeme in
    PattTest.fresh_patt_var ~id ()
  in
  let ty =
    let id = Sym.gen seq ~id:Prim.id_bool in
    TypeTest.fresh_ty_constr ~start:(0, 8, 8) ~id ()
  in
  let value = fresh_atom_bool ~start:(0, 15, 15) ~value:true () in
  fresh_value_binding ~patt ~explicit:true ~ty ~value ()
    |> assert_parses_binding ~ctxt [
         sprintf "%s: %s = true" lexeme Prim.id_bool
       ]

(* Blocks *)

let test_parse_block_let ctxt =
  let seq = Sym.seq () in
  let lexeme = "testId" in
  let id = Sym.gen seq ~id:lexeme in
  let binding =
    let patt = PattTest.fresh_patt_var ~start:(0, 4, 4) ~id () in
    let ty =
      let id = Sym.gen seq ~id:Prim.id_bool in
      TypeTest.fresh_ty_constr ~start:(0, 12, 12) ~id ()
    in
    let value = fresh_atom_bool ~start:(0, 19, 19) ~value:true () in
    fresh_value_binding ~patt ~explicit:true ~ty ~value ()
  in
  let scope = fresh_atom_ident ~start:(0, 27, 27) ~id () in
  fresh_block_let ~binding ~scope ()
    |> assert_parses_block ~ctxt [
         sprintf "let %s: %s = true in %s" lexeme Prim.id_bool lexeme
       ]

let test_parse_block_expr ctxt =
  fresh_atom_bool ~value:true ()
    |> assert_parses_block ~ctxt ["true"]

(* Test Suite *)

let suite =
  "Abstract Syntax" >::: [
    "Runes" >::: [
      (* "Literals"              >:: test_parse_rune_lit; *)
      (* "Escaped Single Quotes" >:: test_parse_rune_lit_esc_squote; *)
      (* "Carriage Return"       >:: test_parse_rune_lit_cr; *)
      (* "Line Feed"             >:: test_parse_rune_lit_lf; *)
      (* "Tab"                   >:: test_parse_rune_lit_tab; *)
    ];
    "Strings" >::: [

    ];
    "Expressions" >::: [
      "Literals" >::: [
        "Booleans" >:: test_parse_lit_bool;
        "Integers" >:: test_parse_lit_int;
        "Longs"    >:: test_parse_lit_long;
        "Floats"   >:: test_parse_lit_float;
        "Doubles"  >:: test_parse_lit_double;
        "Runes"    >:: test_parse_lit_rune;
        "Strings"  >:: test_parse_lit_string;
      ];
      "Identifiers" >::: [
        "Lower Case" >:: test_parse_ident_lident;
        "Upper Case" >:: test_parse_ident_uident;
      ];
      "Atoms" >::: [
        "Literals"                  >:: test_parse_atom_lit;
        "Identifiers"               >:: test_parse_atom_ident;
        "Parenthesized Expressions" >:: test_parse_atom_paren;
      ];
      "Expressions" >::: [
        "Operators" >::: [
          "Unary"  >:: test_parse_expr_un_op;
          "Binary" >:: test_parse_expr_bin_op;
        ];
        "Atoms" >:: test_parse_expr_atom;
      ];
      "Bindings" >::: [
        "Value Bindings" >::: [
          "Implicit" >:: test_parse_binding_value_binding_implicit;
          "Explicit" >:: test_parse_binding_value_binding_explicit;
        ];
      ];
      "Blocks" >::: [
        "Let Bindings" >:: test_parse_block_let;
        "Expressions"  >:: test_parse_block_expr;
      ];
    ];
  ]
