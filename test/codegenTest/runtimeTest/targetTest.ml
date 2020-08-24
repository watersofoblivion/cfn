open Format

open Ctypes
open Llvm
open Llvm_analysis
open Llvm_executionengine

open Runtime

open OUnit2

let _ =
  if initialize ()
  then ()
  else assert_failure "Could not initialize LLVM JIT"

module type Exe = sig
  val ee : llexecutionengine

  val global : 'a typ -> string -> 'a
  val func : 'a typ -> string -> 'a
end

module Compile (Asm: Target.Asm) = struct
  let ee =
    let _ = match verify_module Asm.md with
      | None -> ()
      | Some msg ->
        dump_module Asm.md;
        let msg = sprintf "Invalid module: %s\n%!" msg in
        assert_failure msg
    in
    create Asm.md

  let global ty name = get_global_value_address name ty ee
  let func ty name = get_function_address name ty ee
end

module type Suite = sig
  val suite : test
end
