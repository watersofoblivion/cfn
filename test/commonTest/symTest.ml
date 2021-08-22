open Format

open OUnit2

open Common

(* Helpers *)

let seq = Sym.seq ()
let fresh_sym ?id:(id="") _ = Sym.gen ~id seq

(* Assertions *)

let printer sym =
  Sym.pp sym str_formatter
    |> flush_str_formatter

let assert_sym_equal ~ctxt expected actual =
  actual
    |> assert_equal ~ctxt ~cmp:Sym.equal ~msg:"Symbol indexes are not equal" ~printer expected;
  let printer id =
    pp_print_option (fun fmt id -> fprintf fmt "%s" id) str_formatter id
      |> flush_str_formatter
  in
  let expected = Sym.id expected in
  actual
    |> Sym.id
    |> assert_equal ~ctxt ~msg:"Symbol identifiers are not equal" ~printer expected


let sym_not_equal ~ctxt expected actual =
  let cmp x y = not (Sym.equal x y) in
  actual
    |> assert_equal ~ctxt ~cmp ~msg:"Symbol indexes are equal" ~printer expected;
  let printer id =
    pp_print_option (fun fmt id -> fprintf fmt "%s" id) str_formatter id
      |> flush_str_formatter
  in
  let expected = Sym.id expected in
  actual
    |> Sym.id
    |> assert_equal ~ctxt ~cmp:(!=) ~msg:"Symbol identifiers are equal" ~printer expected

(* Symbols *)

let test_sym_sym ctxt =
  let assert_sym_sym seq expected =
    let actual =
      Sym.gen seq
        |> Sym.sym
    in
    assert_equal ~ctxt ~msg:"Symbol indexes do not match" ~printer:string_of_int expected actual
  in
  let seq = Sym.seq () in
  assert_sym_sym seq 0;
  assert_sym_sym seq 1

let test_sym_id_present ctxt =
  let seq = Sym.seq () in
  let expected = "the-id" in
  let sym = Sym.gen ~id:expected seq in
  match Sym.id sym with
    | Some actual -> assert_equal ~ctxt ~msg:"Symbol identifiers do not match" ~printer:Fun.id expected actual
    | None -> assert_failure "Symbol identifier not present"

let test_sym_id_absent _ =
  let seq = Sym.seq () in
  let sym = Sym.gen seq in
  match Sym.id sym with
    | Some _ -> assert_failure "Symbol identifier present"
    | _ -> ()

let test_sym_equal _ =
  let sym = Sym.gen (Sym.seq ()) in
  Sym.gen (Sym.seq ())
    |> Sym.equal sym
    |> assert_bool "Symbols are not equal"

let test_sym_equal_ignore_id _ =
  let sym = Sym.gen (Sym.seq ()) in
  Sym.gen ~id:"an-identifier" (Sym.seq ())
    |> Sym.equal sym
    |> assert_bool "Symbols are not equal"

let test_sym_unequal _ =
  let seq = Sym.seq () in
  let sym = Sym.gen seq in
  Sym.gen seq
    |> Sym.equal sym
    |> not
    |> assert_bool "Symbols are equal"

let test_sym =
  "Symbols" >::: [
    "Symbol"     >:: test_sym_sym;
    "Identifier" >::: [
      "Present" >:: test_sym_id_present;
      "Absent"  >:: test_sym_id_absent
    ];
    "Equality" >::: [
      "Equality"            >:: test_sym_equal;
      "Ignores Identifiers" >:: test_sym_equal_ignore_id;
      "Inequality"          >:: test_sym_unequal;
    ]
  ]

(* Sequences *)

let test_gen_distinct ctxt =
  let seq = Sym.seq () in
  let sym = Sym.gen seq in
  Sym.gen seq
    |> assert_equal ~ctxt ~cmp:(!=) ~msg:"Symbols are equal" ~printer sym

let test_seq =
  "Sequences" >::: [
    "Generates Distinct Symbols" >:: test_gen_distinct
  ]

(* Pretty Printing *)

let assert_pp = PrettyTest.assert_pp Sym.pp
let assert_pp_id = PrettyTest.assert_pp Sym.pp_id

let test_pp_with_id ctxt =
  ()
    |> Sym.seq
    |> Sym.gen ~id:"ident"
    |> assert_pp ~ctxt ["ident$0"]

let test_pp_without_id ctxt =
  ()
    |> Sym.seq
    |> Sym.gen
    |> assert_pp ~ctxt ["$0"]

let test_pp_id_with_id ctxt =
  ()
    |> Sym.seq
    |> Sym.gen ~id:"ident"
    |> assert_pp_id ~ctxt ["ident"]

let test_pp_id_without_id _ =
  let sym =
    ()
      |> Sym.seq
      |> Sym.gen
  in
  let exn =
    let msg = "Symbol $0 does not have an identifier" in
    Invalid_argument msg
  in
  assert_raises exn (fun _ ->
    Sym.pp_id sym str_formatter)

let test_pp =
  "Pretty Printing" >::: [
    "Complete" >::: [
      "With Identifier"    >:: test_pp_with_id;
      "Without Identifier" >:: test_pp_without_id;
    ];
    "Indentifier Only" >::: [
      "With Identifier"    >:: test_pp_id_with_id;
      "Without Identifier" >:: test_pp_id_without_id;
    ]
  ]

(* Tables *)

let assert_bound ~ctxt sym tbl expected =
  Sym.lookup sym tbl
    |> assert_equal ~ctxt ~msg:"Unexpected symbol bound" ~printer:Fun.id expected

let test_tbl_bind_lookup ctxt =
  let seq = Sym.seq () in
  let sym = Sym.gen seq in
  let expected = "bound-value" in
  Sym.bind sym expected Sym.tbl (fun tbl ->
    assert_bound ~ctxt sym tbl expected)

let test_tbl_bind_non_destructive ctxt =
  let seq = Sym.seq () in
  let sym = Sym.gen seq in
  let masked = "masked-value" in
  Sym.bind sym masked Sym.tbl (fun tbl ->
    assert_bound ~ctxt sym tbl masked;
    let expected = "bound-value" in
    Sym.bind sym expected tbl (fun tbl' ->
      assert_bound ~ctxt sym tbl masked;
      assert_bound ~ctxt sym tbl' expected))

let test_tbl =
  "Tables" >::: [
    "Bind" >::: [
      "Lookup"          >:: test_tbl_bind_lookup;
      "Non-Destructive" >:: test_tbl_bind_non_destructive;
    ]
  ]

(* Suite *)

let suite =
  "Symbolization" >::: [
    test_sym;
    test_seq;
    test_pp;
    test_tbl;
  ]
