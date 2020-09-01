open Runtime

open OUnit2

module type Bindings = sig
  module Types : TypesTest.Bindings
  module Syscall : SyscallTest.Bindings
  module Libc : LibcTest.Bindings
  module Unwind : UnwindTest.Bindings
  module Exn : ExnTest.Bindings
  module Gc : GcTest.Bindings
  module Json : JsonTest.Bindings
  module Xml : XmlTest.Bindings
  module Http : HttpTest.Bindings
end

module Bind (System: System.Asm) (Exe: TargetTest.Exe) = struct
  module Types = TypesTest.Bind (System.Types) (Exe)
  module Syscall = SyscallTest.Bind (System.Syscall) (Exe)
  module Libc = LibcTest.Bind (System.Libc) (Exe)
  module Unwind = UnwindTest.Bind (System.Unwind) (Exe)
  module Exn = ExnTest.Bind (System.Exn) (Exe)
  module Gc = GcTest.Bind (System.Gc) (Exe)
  module Json = JsonTest.Bind (System.Json) (Exe)
  module Xml = XmlTest.Bind (System.Xml) (Exe)
  module Http = HttpTest.Bind (System.Http) (Exe)
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
