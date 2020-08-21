open Llvm

type t = {
  exit: llvalue;
}

let exit syscall = syscall.exit

let generate md =
  let ctx = module_context md in

  let exit =
    let ty = function_type (void_type ctx) [|i32_type ctx|] in
    declare_function "exit" ty md
  in

  { exit = exit }
