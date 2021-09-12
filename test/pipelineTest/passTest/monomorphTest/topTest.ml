(* Top-Level Expressions *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_convert_top_let ctxt =
  let env = EnvTest.fresh () in
  let ir =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Ir.patt_var
    in
    let ty = Ir.ty_bool in
    true
      |> Ir.atom_bool
      |> Ir.expr_atom
      |> Ir.binding patt ty
      |> Ir.top_let
  in
  let id =
    ()
      |> Sym.seq
      |> Sym.gen
  in
  let ty = Mono.ty_bool in
  let mono =
    let patt = Mono.patt_var id in
    true
      |> Mono.atom_bool
      |> Mono.expr_atom
      |> Mono.binding patt ty
      |> Mono.top_let
  in
  Monomorph.convert_top env ir (fun env top ->
    EnvTest.assert_bound ~ctxt MonoTest.assert_ty_equal id env ty;
    MonoTest.assert_top_equal ~ctxt mono top)

(* Test Suite *)

let suite =
  "Top-Level Expressions" >::: [
    "Let Bindings" >:: test_convert_top_let;
  ]
