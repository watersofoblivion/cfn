(* Patterns *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_convert_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let ir = Ir.patt_ground in
  let mono = Mono.patt_ground in
  Monomorph.convert_patt env ir Mono.ty_bool (fun _ patt ->
    MonoTest.assert_patt_equal ~ctxt mono patt)

let test_convert_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Mono.ty_bool in
  let ir = Ir.patt_var id in
  let mono = Mono.patt_var id in
  Monomorph.convert_patt env ir ty (fun env patt ->
    EnvTest.assert_bound ~ctxt MonoTest.assert_ty_equal id env ty;
    MonoTest.assert_patt_equal ~ctxt mono patt)

(* Test Suite *)

let suite =
  "Patterns" >::: [
    "Ground"     >:: test_convert_patt_ground;
    "Identifier" >:: test_convert_patt_var;
  ]
