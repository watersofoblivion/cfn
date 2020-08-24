open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  val exit : unit -> unit
end

module Bind (Asm: Syscall.Asm) (Exe: TargetTest.Exe) = struct
  let exit =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Asm.Names.exit
end

let syscall_test tester ctxt =
  let ctx = Llvm.create_context () in
  let finally _ = Llvm.dispose_context ctx in
  let fn _ =
    let module Target = struct
      module Names = struct
        let prefix = "cfn++"
      end
      let ctx = ctx
      let md = Llvm.create_module ctx "test-module"
    end in

    let module Asm = Syscall.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Syscall = Bind (Asm) (Exe) in
    tester (module Syscall: Bindings) ctxt
  in
  Fun.protect ~finally fn

let _ = syscall_test

let suite =
  "Syscall" >::: [
  ]
