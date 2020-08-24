open Runtime

open OUnit2

module type Bindings = sig
  module Syscall : SyscallTest.Bindings
  module Libc : LibcTest.Bindings
  module Unwind : UnwindTest.Bindings
  module Exn : ExnTest.Bindings
  module Gc : GcTest.Bindings
  module Json : JsonTest.Bindings
  module Xml : XmlTest.Bindings
  module Http : HttpTest.Bindings
end

module Bind (Asm: System.Asm) (Exe: TargetTest.Exe) = struct
  module Syscall = SyscallTest.Bind (Asm.Syscall) (Exe)
  module Libc = LibcTest.Bind (Asm.Libc) (Exe)
  module Unwind = UnwindTest.Bind (Asm.Unwind) (Exe)
  module Exn = ExnTest.Bind (Asm.Exn) (Exe)
  module Gc = GcTest.Bind (Asm.Gc) (Exe)
  module Json = JsonTest.Bind (Asm.Json) (Exe)
  module Xml = XmlTest.Bind (Asm.Xml) (Exe)
  module Http = HttpTest.Bind (Asm.Http) (Exe)
end

let system_test tester ctxt =
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

    let module Asm = System.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module System = Bind (Asm) (Exe) in
    tester (module System: Bindings) ctxt
  in
  Fun.protect ~finally fn

let _ = system_test

let suite =
  "System" >::: [
  ]
