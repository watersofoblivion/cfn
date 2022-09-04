(* Atomic Values *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_mono_atom_bool ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_bool true in
  let mono = Mono.atom_bool true in
  Monomorph.mono_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_bool ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_mono_atom_int ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_int 42l in
  let mono = Mono.atom_int 42l in
  Monomorph.mono_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_int ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_mono_atom_long ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_long 42L in
  let mono = Mono.atom_long 42L in
  Monomorph.mono_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_long ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_mono_atom_float ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_float 4.2 in
  let mono = Mono.atom_float 4.2 in
  Monomorph.mono_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_float ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_mono_atom_double ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.atom_double 4.2 in
  let mono = Mono.atom_double 4.2 in
  Monomorph.mono_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_double ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_mono_atom_rune ctxt =
  let env = EnvTest.fresh () in
  let value = Uchar.of_char 'a' in
  let ir = Ir.atom_rune value in
  let mono = Mono.atom_rune value in
  Monomorph.mono_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_rune ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_mono_atom_string ctxt =
  let env = EnvTest.fresh () in
  let value = "foo bar" in
  let ir = Ir.atom_string value in
  let mono = Mono.atom_string value in
  Monomorph.mono_atom env ir (fun ty atom ->
    MonoTest.assert_ty_equal ~ctxt Mono.ty_string ty;
    MonoTest.assert_atom_equal ~ctxt mono atom)

let test_mono_atom_ident ctxt =
  let value = () |> Sym.seq |> Sym.gen in
  let bound = Mono.ty_bool in
  let env = EnvTest.fresh () in
  let ir = Ir.atom_ident value in
  let mono = Mono.atom_ident value in
  Env.bind value bound env (fun env ->
    Monomorph.mono_atom env ir (fun ty atom ->
      MonoTest.assert_ty_equal ~ctxt bound ty;
      MonoTest.assert_atom_equal ~ctxt mono atom))

let test_mono_atom_ident_unbound _ =
  let value = () |> Sym.seq |> Sym.gen in
  let env = EnvTest.fresh () in
  let ir = Ir.atom_ident value in
  Monomorph.mono_atom env ir
    |> CheckTest.assert_raises_unbound_identifier value

(* Test Suite *)

let suite =
  "Atomic Values" >::: [
    "Booleans"    >:: test_mono_atom_bool;
    "Integers"    >:: test_mono_atom_int;
    "Longs"       >:: test_mono_atom_long;
    "Floats"      >:: test_mono_atom_float;
    "Doubles"     >:: test_mono_atom_double;
    "Runes"       >:: test_mono_atom_rune;
    "Strings"     >:: test_mono_atom_string;
    "Identifiers" >::: [
      "Bound"   >:: test_mono_atom_ident;
      "Unbound" >:: test_mono_atom_ident_unbound;
    ];
  ]
