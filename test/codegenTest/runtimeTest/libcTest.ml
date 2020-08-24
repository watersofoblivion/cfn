open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  val malloc : int64 -> (int64, [`C]) pointer
end

module Bind (Asm: Libc.Asm) (Exe: TargetTest.Exe) = struct
  let malloc =
    let ty = Foreign.funptr (int64_t @-> returning (ptr int64_t)) in
    Exe.func ty Asm.Names.malloc
end

let libc_test tester ctxt =
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

    let module Asm = Libc.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Libc = Bind (Asm) (Exe) in
    tester (module Libc: Bindings) ctxt
  in
  Fun.protect ~finally fn

let _ = libc_test

let suite =
  "Libc" >::: [
  ]
