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

let fresh_rune_escape ?start:(start = ParseUtils.bof) ?value:(value = "\\U+2a") _ =
  let loc = ParseUtils.lexeme_loc start value in
  Syntax.rune_escape loc value

let fresh_str_lit ?start:(start = ParseUtils.bof) ?value:(value = "foo bar") _ =
  let loc = ParseUtils.lexeme_loc start value in
  SyntaxTest.fresh_str_lit ~loc ~value ()

let fresh_str_escape ?start:(start = ParseUtils.bof) ?value:(value = "\\U+2a") _ =
  let loc = ParseUtils.lexeme_loc start value in
  Syntax.str_escape loc value

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

let fresh_atom_string ?start:(start = ParseUtils.bof) ?value:(value = []) _ =
  let rec str_end lines =
    let rec line = function
      | [] -> failwith "Boom!"
      | hd :: [] -> Syntax.loc_str hd
      | _ :: tl -> line tl
    in
    match lines with
      | [] ->
        start
          |> LocTest.make start
          |> LocTest.shift (0, 1, 1)
          |> LocTest.span_from start
      | hd :: [] -> line hd
      | _ :: tl -> str_end tl
  in
  let loc =
    value
      |> str_end
      |> LocTest.shift (0, 1, 1)
      |> LocTest.span_from start
  in
  SyntaxTest.fresh_expr_string ~loc ~value ()

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

let fresh_term_let ?start:(start = ParseUtils.bof) ?binding:(binding = fresh_value_binding ()) ?scope:(scope = fresh_atom_bool ()) _ =
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
let assert_parses_term = ParseUtils.assert_parses Parse.parse_term SyntaxTest.assert_expr_equal

let assert_parses_rune_lit value input ctxt =
  let loc = ParseUtils.lexeme_loc ParseUtils.bof input in
  SyntaxTest.fresh_rune_lit ~loc ~value ()
    |> assert_parses_rune ~ctxt [input]

(* Tests *)

(* Runes *)

let test_parse_rune_lit = assert_parses_rune_lit 'a' "a"
let test_parse_rune_lit_esc_bslash = assert_parses_rune_lit '\\' "\\\\"
let test_parse_rune_lit_esc_squote = assert_parses_rune_lit '\'' "\\'"
let test_parse_rune_lit_cr = assert_parses_rune_lit '\r' "\\r"
let test_parse_rune_lit_lf = assert_parses_rune_lit '\n' "\\n"
let test_parse_rune_lit_tab = assert_parses_rune_lit '\t' "\\t"

let test_parse_rune_escape ctxt =
  let assert_parses_rune_escape value =
    fresh_rune_escape ~value ()
      |> assert_parses_rune ~ctxt [value]
  in
  List.iter assert_parses_rune_escape [
    "\\u2a"; "\\U2a"; "\\u2A"; "\\U2A";
    "\\u+2a"; "\\U+2a"; "\\u+2A"; "\\U+2A";
    "\\U+1"; "\\U+12"; "\\U+123"; "\\U+1234"; "\\U+12345"; "\\U+123456";
  ]

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

let test_parse_lit_rune ctxt =
  let assert_parses_rune value lexeme loc =
    SyntaxTest.fresh_expr_rune ~loc ~value ()
      |> assert_parses_lit ~ctxt [sprintf "'%s'" lexeme]
  in
  let assert_parses_rune_lit value lexeme =
    let lexeme_loc = ParseUtils.lexeme_loc (0, 1, 1) lexeme in
    let value = SyntaxTest.fresh_rune_lit ~loc:lexeme_loc ~value () in
    lexeme_loc
      |> LocTest.shift (0, 1, 1)
      |> LocTest.span_from ParseUtils.bof
      |> assert_parses_rune value lexeme
  in
  let assert_parses_rune_escape lexeme =
    let value = fresh_rune_escape ~start:(0, 1, 1) ~value:lexeme () in
    value
      |> Syntax.loc_rune
      |> LocTest.shift (0, 1, 1)
      |> LocTest.span_from ParseUtils.bof
      |> assert_parses_rune value lexeme
  in
  assert_parses_rune_lit 'a' "a";
  assert_parses_rune_lit '\'' "\\'";
  assert_parses_rune_lit '\\' "\\\\";
  assert_parses_rune_lit '\n' "\\n";
  assert_parses_rune_lit '\r' "\\r";
  assert_parses_rune_lit '\t' "\\t";
  assert_parses_rune_escape "\\U+2a"

let test_parse_lit_string ctxt =
  fresh_atom_string ~value:[] ()
    |> assert_parses_lit ~ctxt ["\"\""]
  (* let seg = fresh_str_lit ~start:(0, 1, 1) ~value:"foo" in
  fresh_atom_str ~value:[[seg]] ()
    |> assert_parses_lit ~ctxt ["\"foo\""]; *)


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
  fresh_term_let ~start:(0, 1, 1) ~binding ~scope ()
    |> assert_parses_atom ~ctxt [
         sprintf "(let %s: %s = true in %s)" lexeme Prim.id_bool lexeme
       ]

(* Expressions *)

type assoc = Left | Right

let pp_assoc fmt = function
  | Left -> fprintf fmt "left"
  | Right -> fprintf fmt "right"

let un_ops = [
  (Syntax.un_neg,     "-");
  (Syntax.un_log_not, "!");
  (Syntax.un_bit_not, "~");
]

let bin_ops = [
  (Right, [
    (Syntax.bin_exp, "^^")]);
  (Left, [
    (Syntax.bin_mul, "*");
    (Syntax.bin_div, "/");
    (Syntax.bin_mod, "%")]);
  (Left, [
    (Syntax.bin_add, "+");
    (Syntax.bin_sub, "-")]);
  (Left, [
    (Syntax.bin_lt,  "<");
    (Syntax.bin_lte, "<=");
    (Syntax.bin_gt,  ">");
    (Syntax.bin_gte, ">=")]);
  (Left, [(Syntax.bin_bit_and, "&")]);
  (Left, [(Syntax.bin_bit_xor, "^")]);
  (Left, [(Syntax.bin_bit_or,  "|")]);
  (Left, [
    (Syntax.bin_struct_eq,  "==");
    (Syntax.bin_struct_neq, "!=");
    (Syntax.bin_phys_eq,    "===");
    (Syntax.bin_phys_neq,   "!==")
  ]);
  (Left, [(Syntax.bin_log_and, "&&")]);
  (Left, [(Syntax.bin_log_or,  "||")]);
]

let test_parse_expr_un_op ctxt =
  List.iter (fun (constr, lexeme) ->
    let op_len = String.length lexeme in
    let op =
      LocTest.make (0, 0, 0) (0, op_len, op_len)
        |> constr
    in
    let operand = fresh_atom_int ~start:(0, 1 + op_len, 1 + op_len) ~lexeme:"1" () in
    fresh_un_op ~op ~operand ()
      |> assert_parses_expr ~ctxt [sprintf "%s 1" lexeme]
  ) un_ops

let test_parse_expr_bin_op ctxt =
  let left = fresh_atom_int ~lexeme:"1" () in
  let rec versus acc = function
    | [] -> ()
    | ((assoc, ops) as hd) :: tl ->
      let rec versus' acc' = function
        | [] -> ()
        | ((constr, lexeme) as hd') :: tl' ->
          (* Self *)
          let op_len = String.length lexeme in
          let left_op_loc = LocTest.make (0, 2, 2) (0, 2 + op_len, 2 + op_len) in
          let right_op_loc = LocTest.make (0, 5 + op_len, 5 + op_len) (0, 5 + 2*op_len, 5 + 2*op_len) in
          let center = fresh_atom_int ~start:(0, 3 + op_len, 3 + op_len) ~lexeme:"2" () in
          let right = fresh_atom_int ~start:(0, 6 + 2*op_len, 6 + 2*op_len) ~lexeme:"3" () in
          let expr = match assoc with
            | Left ->
              let lhs =
                let op = constr left_op_loc in
                fresh_bin_op ~op ~lhs:left ~rhs:center ()
              in
              let op = constr right_op_loc in
              fresh_bin_op ~op ~lhs ~rhs:right ()
            | Right ->
              let rhs =
                let op = constr right_op_loc in
                fresh_bin_op ~op ~lhs:center ~rhs:right ()
              in
              let op = constr left_op_loc in
              fresh_bin_op ~op ~lhs:left ~rhs ()
          in
          expr
            |> assert_parses_expr ~ctxt [sprintf "1 %s 2 %s 3" lexeme lexeme];

          (* Other operators of equal precedence *)
          List.iter (fun (constr', lexeme') ->
            let op_len' = String.length lexeme' in
            let left_op_loc = LocTest.make (0, 2, 2) (0, 2 + op_len, 2 + op_len) in
            let right_op_loc = LocTest.make (0, 5 + op_len, 5 + op_len) (0, 5 + op_len + op_len', 5 + op_len + op_len') in
            let center = fresh_atom_int ~start:(0, 3 + op_len, 3 + op_len) ~lexeme:"2" () in
            let right = fresh_atom_int ~start:(0, 6 + op_len + op_len', 6 + op_len + op_len') ~lexeme:"3" () in
            let expr = match assoc with
              | Left ->
                let lhs =
                  let op = constr left_op_loc in
                  fresh_bin_op ~op ~lhs:left ~rhs:center ()
                in
                let op = constr' right_op_loc in
                fresh_bin_op ~op ~lhs ~rhs:right ()
              | Right ->
                let rhs =
                  let op = constr' right_op_loc in
                  fresh_bin_op ~op ~lhs:center ~rhs:right ()
                in
                let op = constr left_op_loc in
                fresh_bin_op ~op ~lhs:left ~rhs ()
            in
            expr
              |> assert_parses_expr ~ctxt [sprintf "1 %s 2 %s 3" lexeme lexeme'];

            let left_op_loc = LocTest.make (0, 2, 2) (0, 2 + op_len', 2 + op_len') in
            let right_op_loc = LocTest.make (0, 5 + op_len', 5 + op_len') (0, 5 + op_len + op_len', 5 + op_len + op_len') in
            let center = fresh_atom_int ~start:(0, 3 + op_len', 3 + op_len') ~lexeme:"2" () in
            let expr = match assoc with
              | Left ->
                let lhs =
                  let op = constr' left_op_loc in
                  fresh_bin_op ~op ~lhs:left ~rhs:center ()
                in
                let op = constr right_op_loc in
                fresh_bin_op ~op ~lhs ~rhs:right ()
              | Right ->
                let rhs =
                  let op = constr right_op_loc in
                  fresh_bin_op ~op ~lhs:center ~rhs:right ()
                in
                let op = constr' left_op_loc in
                fresh_bin_op ~op ~lhs:left ~rhs ()
            in
            expr
              |> assert_parses_expr ~ctxt [sprintf "1 %s 2 %s 3" lexeme' lexeme];
          ) tl';

          (* Higher Precedence Operators *)
          List.iter (fun (_, higher) ->
            List.iter (fun (constr', lexeme') ->
              let op_len' = String.length lexeme' in
              let left_op_loc = LocTest.make (0, 2, 2) (0, 2 + op_len, 2 + op_len) in
              let right_op_loc = LocTest.make (0, 5 + op_len, 5 + op_len) (0, 5 + op_len + op_len', 5 + op_len + op_len') in
              let center = fresh_atom_int ~start:(0, 3 + op_len, 3 + op_len) ~lexeme:"2" () in
              let right = fresh_atom_int ~start:(0, 6 + op_len + op_len', 6 + op_len + op_len') ~lexeme:"3" () in
              let rhs =
                let op = constr' right_op_loc in
                fresh_bin_op ~op ~lhs:center ~rhs:right ()
              in
              let op = constr left_op_loc in
              fresh_bin_op ~op ~lhs:left ~rhs ()
                |> assert_parses_expr ~ctxt [sprintf "1 %s 2 %s 3" lexeme lexeme'];

              let left_op_loc = LocTest.make (0, 2, 2) (0, 2 + op_len', 2 + op_len') in
              let right_op_loc = LocTest.make (0, 5 + op_len', 5 + op_len') (0, 5 + op_len + op_len', 5 + op_len + op_len') in
              let center = fresh_atom_int ~start:(0, 3 + op_len', 3 + op_len') ~lexeme:"2" () in
              let lhs =
                let op = constr' left_op_loc in
                fresh_bin_op ~op ~lhs:left ~rhs:center ()
              in
              let op = constr right_op_loc in
              fresh_bin_op ~op ~lhs ~rhs:right ()
                |> assert_parses_expr ~ctxt [sprintf "1 %s 2 %s 3" lexeme' lexeme];
            ) higher
          ) acc;

          (* Lower Precedence Operators *)
          List.iter (fun (_, lower) ->
            List.iter (fun (constr', lexeme') ->
              let op_len' = String.length lexeme' in
              let left_op_loc = LocTest.make (0, 2, 2) (0, 2 + op_len, 2 + op_len) in
              let right_op_loc = LocTest.make (0, 5 + op_len, 5 + op_len) (0, 5 + op_len + op_len', 5 + op_len + op_len') in
              let center = fresh_atom_int ~start:(0, 3 + op_len, 3 + op_len) ~lexeme:"2" () in
              let right = fresh_atom_int ~start:(0, 6 + op_len + op_len', 6 + op_len + op_len') ~lexeme:"3" () in
              let lhs =
                let op = constr left_op_loc in
                fresh_bin_op ~op ~lhs:left ~rhs:center ()
              in
              let op = constr' right_op_loc in
              fresh_bin_op ~op ~lhs ~rhs:right ()
                |> assert_parses_expr ~ctxt [sprintf "1 %s 2 %s 3" lexeme lexeme'];

              let left_op_loc = LocTest.make (0, 2, 2) (0, 2 + op_len', 2 + op_len') in
              let right_op_loc = LocTest.make (0, 5 + op_len', 5 + op_len') (0, 5 + op_len + op_len', 5 + op_len + op_len') in
              let center = fresh_atom_int ~start:(0, 3 + op_len', 3 + op_len') ~lexeme:"2" () in
              let rhs =
                let op = constr right_op_loc in
                fresh_bin_op ~op ~lhs:center ~rhs:right ()
              in
              let op = constr' left_op_loc in
              fresh_bin_op ~op ~lhs:left ~rhs ()
                |> assert_parses_expr ~ctxt [sprintf "1 %s 2 %s 3" lexeme' lexeme];
            ) lower
          ) tl;

          versus' (hd' :: acc') tl'
    in
    versus' [] ops;
    versus (hd :: acc) tl
  in
  versus [] bin_ops

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

(* Terms *)

let test_parse_term_let ctxt =
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
  fresh_term_let ~binding ~scope ()
    |> assert_parses_term ~ctxt [
         sprintf "let %s: %s = true in %s" lexeme Prim.id_bool lexeme
       ]

let test_parse_term_expr ctxt =
  fresh_atom_bool ~value:true ()
    |> assert_parses_term ~ctxt ["true"]

(* Test Suite *)

let suite =
  "Abstract Syntax" >::: [
    "Runes" >::: [
      "Literals" >:: test_parse_rune_lit;
      "Escapes" >::: [
        "Backslash"       >:: test_parse_rune_lit_esc_bslash;
        "Single Quotes"   >:: test_parse_rune_lit_esc_squote;
        "Carriage Return" >:: test_parse_rune_lit_cr;
        "Line Feed"       >:: test_parse_rune_lit_lf;
        "Tab"             >:: test_parse_rune_lit_tab;
      ];
      "Unicode Escape Sequences" >:: test_parse_rune_escape;
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
      "Terms" >::: [
        "Let Bindings" >:: test_parse_term_let;
        "Expressions"  >:: test_parse_term_expr;
      ];
    ];
  ]
