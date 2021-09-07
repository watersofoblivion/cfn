open OUnit2

open CommonTest

(* Fixtures *)

let fresh_op_un ?loc:(loc = LocTest.gen ()) _ =
  Syntax.un_neg loc

let fresh_op_bin ?loc:(loc = LocTest.gen ()) _ =
  Syntax.bin_add loc

(* Utilities *)

let deloc_un = function
  | Syntax.UnNeg _ -> Syntax.un_neg LocTest.dummy
  | Syntax.UnLogNot _ -> Syntax.un_log_not LocTest.dummy
  | Syntax.UnBitNot _ -> Syntax.un_bit_not LocTest.dummy

let deloc_bin = function
  | Syntax.BinStructEq _ -> Syntax.bin_struct_eq LocTest.dummy
  | Syntax.BinStructNeq _ -> Syntax.bin_struct_neq LocTest.dummy
  | Syntax.BinPhysEq _ -> Syntax.bin_phys_eq LocTest.dummy
  | Syntax.BinPhysNeq _ -> Syntax.bin_phys_neq LocTest.dummy
  | Syntax.BinLt _ -> Syntax.bin_lt LocTest.dummy
  | Syntax.BinLte _ -> Syntax.bin_lte LocTest.dummy
  | Syntax.BinGt _ -> Syntax.bin_gt LocTest.dummy
  | Syntax.BinGte _ -> Syntax.bin_gte LocTest.dummy
  | Syntax.BinAdd _ -> Syntax.bin_add LocTest.dummy
  | Syntax.BinSub _ -> Syntax.bin_sub LocTest.dummy
  | Syntax.BinMul _ -> Syntax.bin_mul LocTest.dummy
  | Syntax.BinDiv _ -> Syntax.bin_div LocTest.dummy
  | Syntax.BinMod _ -> Syntax.bin_mod LocTest.dummy
  | Syntax.BinExp _ -> Syntax.bin_exp LocTest.dummy
  | Syntax.BinLogAnd _ -> Syntax.bin_log_and LocTest.dummy
  | Syntax.BinLogOr _ -> Syntax.bin_log_or LocTest.dummy
  | Syntax.BinBitAnd _ -> Syntax.bin_bit_and LocTest.dummy
  | Syntax.BinBitOr _ -> Syntax.bin_bit_or LocTest.dummy
  | Syntax.BinBitXor _ -> Syntax.bin_bit_xor LocTest.dummy

(* Assertions *)

let un_not_equal = TestUtils.not_equal "Unary operators" Syntax.pp_un
let bin_not_equal = TestUtils.not_equal "Binary operators" Syntax.pp_bin

let assert_un_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.UnNeg expected, Syntax.UnNeg actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.UnLogNot expected, Syntax.UnLogNot actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.UnBitNot expected, Syntax.UnBitNot actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | expected, actual -> un_not_equal ~ctxt expected actual

let assert_bin_equal ~ctxt expected actual = match (expected, actual) with
  | Syntax.BinStructEq expected, Syntax.BinStructEq actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinStructNeq expected, Syntax.BinStructNeq actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinPhysEq expected, Syntax.BinPhysEq actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinPhysNeq expected, Syntax.BinPhysNeq actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinLt expected, Syntax.BinLt actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinLte expected, Syntax.BinLte actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinGt expected, Syntax.BinGt actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinGte expected, Syntax.BinGte actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinAdd expected, Syntax.BinAdd actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinSub expected, Syntax.BinSub actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinMul expected, Syntax.BinMul actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinDiv expected, Syntax.BinDiv actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinMod expected, Syntax.BinMod actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinExp expected, Syntax.BinExp actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinLogAnd expected, Syntax.BinLogAnd actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinLogOr expected, Syntax.BinLogOr actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinBitAnd expected, Syntax.BinBitAnd actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinBitOr expected, Syntax.BinBitOr actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | Syntax.BinBitXor expected, Syntax.BinBitXor actual ->
    LocTest.assert_loc_equal ~ctxt expected.loc actual.loc
  | expected, actual -> bin_not_equal ~ctxt expected actual

(* Tests *)

(* Constructors *)

let test_un_neg ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.un_neg loc in
  match expected with
    | Syntax.UnNeg actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> un_not_equal ~ctxt expected actual

let test_un_log_not ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.un_log_not loc in
  match expected with
    | Syntax.UnLogNot actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> un_not_equal ~ctxt expected actual

let test_un_bit_not ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.un_bit_not loc in
  match expected with
    | Syntax.UnBitNot actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> un_not_equal ~ctxt expected actual

let test_bin_struct_eq ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_struct_eq loc in
  match expected with
    | Syntax.BinStructEq actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_struct_neq ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_struct_neq loc in
  match expected with
    | Syntax.BinStructNeq actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_phys_eq ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_phys_eq loc in
  match expected with
    | Syntax.BinPhysEq actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_phys_neq ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_phys_neq loc in
  match expected with
    | Syntax.BinPhysNeq actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_lt ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_lt loc in
  match expected with
    | Syntax.BinLt actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_lte ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_lte loc in
  match expected with
    | Syntax.BinLte actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_gt ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_gt loc in
  match expected with
    | Syntax.BinGt actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_gte ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_gte loc in
  match expected with
    | Syntax.BinGte actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_add ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_add loc in
  match expected with
    | Syntax.BinAdd actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_sub ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_sub loc in
  match expected with
    | Syntax.BinSub actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_mul ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_mul loc in
  match expected with
    | Syntax.BinMul actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_div ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_div loc in
  match expected with
    | Syntax.BinDiv actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_mod ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_mod loc in
  match expected with
    | Syntax.BinMod actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_exp ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_exp loc in
  match expected with
    | Syntax.BinExp actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_log_and ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_log_and loc in
  match expected with
    | Syntax.BinLogAnd actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_log_or ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_log_or loc in
  match expected with
    | Syntax.BinLogOr actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_bit_and ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_bit_and loc in
  match expected with
    | Syntax.BinBitAnd actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_bit_or ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_bit_or loc in
  match expected with
    | Syntax.BinBitOr actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_bin_bit_xor ctxt =
  let loc = LocTest.gen () in
  let expected = Syntax.bin_bit_xor loc in
  match expected with
    | Syntax.BinBitXor actual ->
      LocTest.assert_loc_equal ~ctxt loc actual.loc
    | actual -> bin_not_equal ~ctxt expected actual

let test_constructor =
  "Constructors" >::: [
    "Comparison" >::: [
      "Equality" >::: [
        "Structural" >::: [
          "Equal"     >:: test_bin_struct_eq;
          "Not Equal" >:: test_bin_struct_neq;
        ];
        "Physical" >::: [
          "Equal"     >:: test_bin_phys_eq;
          "Not Equal" >:: test_bin_phys_neq;
        ];
      ];
      "Less Than"             >:: test_bin_lt;
      "Less Than or Equal"    >:: test_bin_lte;
      "Greater Than"          >:: test_bin_gt;
      "Greater Than or Equal" >:: test_bin_gte;
    ];
    "Arithmetic" >::: [
      "Negation"       >:: test_un_neg;
      "Addition"       >:: test_bin_add;
      "Subtraction"    >:: test_bin_sub;
      "Multiplication" >:: test_bin_mul;
      "Division"       >:: test_bin_div;
      "Modulus"        >:: test_bin_mod;
      "Exponentiation" >:: test_bin_exp;
    ];
    "Logic" >::: [
      "AND" >::: [
        "Logical" >:: test_bin_log_and;
        "Bitwise" >:: test_bin_bit_and;
      ];
      "OR" >::: [
        "Logical" >:: test_bin_log_or;
        "Bitwise" >:: test_bin_bit_or;
      ];
      "NOT" >::: [
        "Logical" >:: test_un_log_not;
        "Bitwise" >:: test_un_bit_not;
      ];
      "XOR" >::: [
        "Bitwise" >:: test_bin_bit_xor;
      ];
    ];
  ]

(* Locations *)

let assert_loc_un = SyntaxUtils.assert_loc Syntax.loc_un
let assert_loc_bin = SyntaxUtils.assert_loc Syntax.loc_bin

let test_loc_un_neg = assert_loc_un Syntax.un_neg
let test_loc_un_log_not = assert_loc_un Syntax.un_log_not
let test_loc_un_bit_not = assert_loc_un Syntax.un_bit_not

let test_loc_bin_struct_eq = assert_loc_bin Syntax.bin_struct_eq
let test_loc_bin_struct_neq = assert_loc_bin Syntax.bin_struct_neq
let test_loc_bin_phys_eq = assert_loc_bin Syntax.bin_phys_eq
let test_loc_bin_phys_neq = assert_loc_bin Syntax.bin_phys_neq
let test_loc_bin_lt = assert_loc_bin Syntax.bin_lt
let test_loc_bin_lte = assert_loc_bin Syntax.bin_lte
let test_loc_bin_gt = assert_loc_bin Syntax.bin_gt
let test_loc_bin_gte = assert_loc_bin Syntax.bin_gte
let test_loc_bin_add = assert_loc_bin Syntax.bin_add
let test_loc_bin_sub = assert_loc_bin Syntax.bin_sub
let test_loc_bin_mul = assert_loc_bin Syntax.bin_mul
let test_loc_bin_div = assert_loc_bin Syntax.bin_div
let test_loc_bin_mod = assert_loc_bin Syntax.bin_mod
let test_loc_bin_exp = assert_loc_bin Syntax.bin_exp
let test_loc_bin_log_and = assert_loc_bin Syntax.bin_log_and
let test_loc_bin_log_or = assert_loc_bin Syntax.bin_log_or
let test_loc_bin_bit_and = assert_loc_bin Syntax.bin_bit_and
let test_loc_bin_bit_or = assert_loc_bin Syntax.bin_bit_or
let test_loc_bin_bit_xor = assert_loc_bin Syntax.bin_bit_xor

let test_loc =
  "Locations" >::: [
    "Comparison" >::: [
      "Equality" >::: [
        "Structural" >::: [
          "Equal"     >:: test_loc_bin_struct_eq;
          "Not Equal" >:: test_loc_bin_struct_neq;
        ];
        "Physical" >::: [
          "Equal"     >:: test_loc_bin_phys_eq;
          "Not Equal" >:: test_loc_bin_phys_neq;
        ];
      ];
      "Less Than"             >:: test_loc_bin_lt;
      "Less Than or Equal"    >:: test_loc_bin_lte;
      "Greater Than"          >:: test_loc_bin_gt;
      "Greater Than or Equal" >:: test_loc_bin_gte;
    ];
    "Arithmetic" >::: [
      "Negation"       >:: test_loc_un_neg;
      "Addition"       >:: test_loc_bin_add;
      "Subtraction"    >:: test_loc_bin_sub;
      "Multiplication" >:: test_loc_bin_mul;
      "Division"       >:: test_loc_bin_div;
      "Modulus"        >:: test_loc_bin_mod;
      "Exponentiation" >:: test_loc_bin_exp;
    ];
    "Logic" >::: [
      "AND" >::: [
        "Logical" >:: test_loc_bin_log_and;
        "Bitwise" >:: test_loc_bin_bit_and;
      ];
      "OR" >::: [
        "Logical" >:: test_loc_bin_log_or;
        "Bitwise" >:: test_loc_bin_bit_or;
      ];
      "NOT" >::: [
        "Logical" >:: test_loc_un_log_not;
        "Bitwise" >:: test_loc_un_bit_not;
      ];
      "XOR" >::: [
        "Bitwise" >:: test_loc_bin_bit_xor;
      ];
    ];
  ]

(* Pretty Printing *)

let assert_pp pp constr lexeme ctxt =
  ()
    |> LocTest.gen
    |> constr
    |> PrettyTest.assert_pp pp ~ctxt [lexeme]

let assert_pp_un = assert_pp Syntax.pp_un
let assert_pp_bin = assert_pp Syntax.pp_bin

let test_pp_un_neg = assert_pp_un Syntax.un_neg "-"
let test_pp_un_log_not = assert_pp_un Syntax.un_log_not "!"
let test_pp_un_bit_not = assert_pp_un Syntax.un_bit_not "~"

let test_pp_bin_struct_eq = assert_pp_bin Syntax.bin_struct_eq "=="
let test_pp_bin_struct_neq = assert_pp_bin Syntax.bin_struct_neq "!="
let test_pp_bin_phys_eq = assert_pp_bin Syntax.bin_phys_eq "==="
let test_pp_bin_phys_neq = assert_pp_bin Syntax.bin_phys_neq "!=="

let test_pp_bin_lt = assert_pp_bin Syntax.bin_lt "<"
let test_pp_bin_lte = assert_pp_bin Syntax.bin_lte "<="
let test_pp_bin_gt = assert_pp_bin Syntax.bin_gt ">"
let test_pp_bin_gte = assert_pp_bin Syntax.bin_gte ">="

let test_pp_bin_add = assert_pp_bin Syntax.bin_add "+"
let test_pp_bin_sub = assert_pp_bin Syntax.bin_sub "-"
let test_pp_bin_mul = assert_pp_bin Syntax.bin_mul "*"
let test_pp_bin_div = assert_pp_bin Syntax.bin_div "/"
let test_pp_bin_mod = assert_pp_bin Syntax.bin_mod "%"
let test_pp_bin_exp = assert_pp_bin Syntax.bin_exp "^^"

let test_pp_bin_log_and = assert_pp_bin Syntax.bin_log_and "&&"
let test_pp_bin_log_or = assert_pp_bin Syntax.bin_log_or "||"
let test_pp_bin_bit_and = assert_pp_bin Syntax.bin_bit_and "&"
let test_pp_bin_bit_or = assert_pp_bin Syntax.bin_bit_or "|"
let test_pp_bin_bit_xor = assert_pp_bin Syntax.bin_bit_xor "^"

let test_pp =
  "Pretty Printing" >::: [
    "Comparison" >::: [
      "Equality" >::: [
        "Structural" >::: [
          "Equal"     >:: test_pp_bin_struct_eq;
          "Not Equal" >:: test_pp_bin_struct_neq;
        ];
        "Physical" >::: [
          "Equal"     >:: test_pp_bin_phys_eq;
          "Not Equal" >:: test_pp_bin_phys_neq;
        ];
      ];
      "Less Than"             >:: test_pp_bin_lt;
      "Less Than or Equal"    >:: test_pp_bin_lte;
      "Greater Than"          >:: test_pp_bin_gt;
      "Greater Than or Equal" >:: test_pp_bin_gte;
    ];
    "Arithmetic" >::: [
      "Negation"       >:: test_pp_un_neg;
      "Addition"       >:: test_pp_bin_add;
      "Subtraction"    >:: test_pp_bin_sub;
      "Multiplication" >:: test_pp_bin_mul;
      "Division"       >:: test_pp_bin_div;
      "Modulus"        >:: test_pp_bin_mod;
      "Exponentiation" >:: test_pp_bin_exp;
    ];
    "Logic" >::: [
      "AND" >::: [
        "Logical" >:: test_pp_bin_log_and;
        "Bitwise" >:: test_pp_bin_bit_and;
      ];
      "OR" >::: [
        "Logical" >:: test_pp_bin_log_or;
        "Bitwise" >:: test_pp_bin_bit_or;
      ];
      "NOT" >::: [
        "Logical" >:: test_pp_un_log_not;
        "Bitwise" >:: test_pp_un_bit_not;
      ];
      "XOR" >::: [
        "Bitwise" >:: test_pp_bin_bit_xor;
      ];
    ];
  ]

(* Test Suite *)

let suite =
  "Operators" >::: [
    test_constructor;
    test_loc;
    test_pp;
  ]
