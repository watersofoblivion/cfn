open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  type e

  val raise_exception : e -> unit
end

module Bind (Asm: Unwind.Asm) (Exe: TargetTest.Exe) = struct
  type e = unit

  let raise_exception =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Asm.Names.raise_exception
end

let unwind_test tester ctxt =
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

    let module Libc = Libc.Generate (Target) in

    let module Asm = Unwind.Generate (Libc) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Unwind = Bind (Asm) (Exe) in
    tester (module Unwind: Bindings) ctxt
  in
  Fun.protect ~finally fn

let _ = unwind_test

let suite =
  "Unwind" >::: [
  ]
