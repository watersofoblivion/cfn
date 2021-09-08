(* Patterns *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_convert_patt_ground ctxt =
  let env = EnvTest.fresh () in
  let mono = Mono.patt_ground in
  let clos = Clos.patt_ground in
  Conv.convert_patt env mono Clos.ty_bool (fun _ patt ->
    ClosTest.assert_patt_equal ~ctxt clos patt)

let test_convert_patt_var ctxt =
  let env = EnvTest.fresh () in
  let id = () |> Sym.seq |> Sym.gen in
  let ty = Clos.ty_bool in
  let mono = Mono.patt_var id in
  let clos = Clos.patt_var id in
  Conv.convert_patt env mono ty (fun env patt ->
    EnvTest.assert_bound ~ctxt ClosTest.assert_ty_equal id env ty;
    ClosTest.assert_patt_equal ~ctxt clos patt)

(* Test Suite *)

let suite =
  "Patterns" >::: [
    "Ground"     >:: test_convert_patt_ground;
    "Identifier" >:: test_convert_patt_var;
  ]
