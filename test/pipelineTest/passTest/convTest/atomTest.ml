(* Atomic Values *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_convert_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_bool true in
  let clos = Clos.atom_bool true in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_bool ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_int ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_int 42l in
  let clos = Clos.atom_int 42l in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_int ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_long ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_long 42L in
  let clos = Clos.atom_long 42L in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_long ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_float ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_float 4.2 in
  let clos = Clos.atom_float 4.2 in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_float ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_double ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.atom_double 4.2 in
  let clos = Clos.atom_double 4.2 in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_double ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let mono = Mono.atom_rune value in
  let clos = Clos.atom_rune value in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_rune ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_string ctxt =
  let env = EnvTest.fresh () in
  let value = "foo bar" in
  let mono = Mono.atom_string value in
  let clos = Clos.atom_string value in
  Conv.convert_atom env mono (fun ty atom ->
    ClosTest.assert_ty_equal ~ctxt Clos.ty_string ty;
    ClosTest.assert_atom_equal ~ctxt clos atom)

let test_convert_atom_ident ctxt =
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Clos.ty_bool in
  let env = EnvTest.fresh () in
  let mono = Mono.atom_ident value in
  let clos = Clos.atom_ident value in
  Env.bind value bound env (fun env ->
    Conv.convert_atom env mono (fun ty atom ->
      ClosTest.assert_ty_equal ~ctxt bound ty;
      ClosTest.assert_atom_equal ~ctxt clos atom))

let test_convert_atom_ident_unbound _ =
  let value = () |> Sym.seq |> Sym.gen in
  let env = EnvTest.fresh () in
  let mono = Mono.atom_ident value in
  let exn = Conv.UnboundIdentifier value in
  assert_raises exn (fun _ ->
    Conv.convert_atom env mono (fun _ _ ->
      assert_failure "Expected exception"))

(* Test Suite *)

let suite =
  "Atomic Values" >::: [
    "Booleans"    >:: test_convert_atom_bool;
    "Integers"    >:: test_convert_atom_int;
    "Longs"       >:: test_convert_atom_long;
    "Floats"      >:: test_convert_atom_float;
    "Doubles"     >:: test_convert_atom_double;
    "Runes"       >:: test_convert_atom_rune;
    "Strings"     >:: test_convert_atom_string;
    "Identifiers" >::: [
      "Bound"   >:: test_convert_atom_ident;
      "Unbound" >:: test_convert_atom_ident_unbound;
    ];
  ]
