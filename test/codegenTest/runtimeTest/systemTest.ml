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

let system_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Asm = System.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module System = Bind (Asm) (Exe) in
    test_fn (module System: Bindings))

let _ = system_test

let suite =
  "System" >::: [
  ]
