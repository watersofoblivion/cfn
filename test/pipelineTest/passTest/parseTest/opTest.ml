(* Operators *)

open OUnit2

(* Assertions *)

let assert_parses_op ?start:(start = ParseUtils.bof) parser assert_equal constr op ctxt =
  let assert_parses = ParseUtils.assert_parses parser assert_equal in
  op
    |> ParseUtils.lexeme_loc start
    |> constr
    |> assert_parses ~ctxt [op]

let assert_parses_un = assert_parses_op Parse.parse_un SyntaxTest.assert_un_equal
let assert_parses_bin = assert_parses_op Parse.parse_bin SyntaxTest.assert_bin_equal

(* Tests *)

let test_parse_un_neg = assert_parses_un Syntax.un_neg "-"
let test_parse_un_log_not = assert_parses_un Syntax.un_log_not "!"
let test_parse_un_bit_not = assert_parses_un Syntax.un_bit_not "~"

let test_parse_bin_struct_eq = assert_parses_bin Syntax.bin_struct_eq "=="
let test_parse_bin_struct_neq = assert_parses_bin Syntax.bin_struct_neq "!="
let test_parse_bin_phys_eq = assert_parses_bin Syntax.bin_phys_eq "==="
let test_parse_bin_phys_neq = assert_parses_bin Syntax.bin_phys_neq "!=="

let test_parse_bin_lt = assert_parses_bin Syntax.bin_lt "<"
let test_parse_bin_lte = assert_parses_bin Syntax.bin_lte "<="
let test_parse_bin_gt = assert_parses_bin Syntax.bin_gt ">"
let test_parse_bin_gte = assert_parses_bin Syntax.bin_gte ">="

let test_parse_bin_lsl = assert_parses_bin Syntax.bin_lsl "<<"
let test_parse_bin_lsr = assert_parses_bin Syntax.bin_lsr ">>"
let test_parse_bin_asl = assert_parses_bin Syntax.bin_asl "<<<"
let test_parse_bin_asr = assert_parses_bin Syntax.bin_asr ">>>"

let test_parse_bin_add = assert_parses_bin Syntax.bin_add "+"
let test_parse_bin_sub = assert_parses_bin Syntax.bin_sub "-"
let test_parse_bin_mul = assert_parses_bin Syntax.bin_mul "*"
let test_parse_bin_div = assert_parses_bin Syntax.bin_div "/"
let test_parse_bin_mod = assert_parses_bin Syntax.bin_mod "%"
let test_parse_bin_exp = assert_parses_bin Syntax.bin_exp "^^"

let test_parse_bin_log_and = assert_parses_bin Syntax.bin_log_and "&&"
let test_parse_bin_log_or = assert_parses_bin Syntax.bin_log_or "||"

let test_parse_bin_bit_and = assert_parses_bin Syntax.bin_bit_and "&"
let test_parse_bin_bit_or = assert_parses_bin Syntax.bin_bit_or "|"
let test_parse_bin_bit_xor = assert_parses_bin Syntax.bin_bit_xor "^"

(* Test Suite *)

let suite =
  "Operators" >::: [
    "Comparison" >::: [
      "Equality" >::: [
        "Structural" >::: [
          "Equality"   >:: test_parse_bin_struct_eq;
          "Inequality" >:: test_parse_bin_struct_neq;
        ];
        "Physical" >::: [
          "Equality"   >:: test_parse_bin_phys_eq;
          "Inequality" >:: test_parse_bin_phys_neq;
        ];
      ];
      "Less Than"             >:: test_parse_bin_lt;
      "Less Than or Equal"    >:: test_parse_bin_lte;
      "Greater Than"          >:: test_parse_bin_gt;
      "Greater Than or Equal" >:: test_parse_bin_gte;
    ];
    "Shift" >::: [
      "Logical" >::: [
        "Left"  >:: test_parse_bin_lsl;
        "Right" >:: test_parse_bin_lsr;
      ];
      "Arithmetic" >::: [
        "Left"  >:: test_parse_bin_asl;
        "Right" >:: test_parse_bin_asr;
      ];
    ];
    "Arithmetic" >::: [
      "Negation"       >:: test_parse_un_neg;
      "Addition"       >:: test_parse_bin_add;
      "Subtraction"    >:: test_parse_bin_sub;
      "Multiplication" >:: test_parse_bin_mul;
      "Division"       >:: test_parse_bin_div;
      "Modulus"        >:: test_parse_bin_mod;
      "Exponentiation" >:: test_parse_bin_exp;
    ];
    "Logic" >::: [
      "AND" >::: [
        "Logical" >:: test_parse_bin_log_and;
        "Bitwise" >:: test_parse_bin_bit_and;
      ];
      "OR" >::: [
        "Logical" >:: test_parse_bin_log_or;
        "Bitwise" >:: test_parse_bin_bit_or;
      ];
      "NOT" >::: [
        "Logical" >:: test_parse_un_log_not;
        "Bitwise" >:: test_parse_un_bit_not;
      ];
      "XOR" >::: [
        "Bitwise" >:: test_parse_bin_bit_xor;
      ];
    ];
  ]
