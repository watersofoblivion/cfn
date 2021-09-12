(* Top-Level Expressions *)

open OUnit2

open Common

open CommonTest

(* Tests *)

let test_convert_top_let ctxt =
  let env = EnvTest.fresh () in
  let mono =
    let patt =
      ()
        |> Sym.seq
        |> Sym.gen
        |> Mono.patt_var
    in
    let ty = Mono.ty_bool in
    true
      |> Mono.atom_bool
      |> Mono.expr_atom
      |> Mono.binding patt ty
      |> Mono.top_let
  in
  let id =
    ()
      |> Sym.seq
      |> Sym.gen
  in
  let ty = Clos.ty_bool in
  let clos =
    let patt = Clos.patt_var id in
    true
      |> Clos.atom_bool
      |> Clos.expr_atom
      |> Clos.binding patt ty
      |> Clos.top_let
  in
  Conv.convert_top env mono (fun env top ->
    EnvTest.assert_bound ~ctxt ClosTest.assert_ty_equal id env ty;
    ClosTest.assert_top_equal ~ctxt clos top)

(* Test Suite *)

let suite =
  "Top-Level Expressions" >::: [
    "Let Bindings" >:: test_convert_top_let;
  ]
