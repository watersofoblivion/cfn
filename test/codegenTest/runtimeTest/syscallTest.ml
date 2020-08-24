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

let syscall_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Asm = Syscall.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Syscall = Bind (Asm) (Exe) in
    test_fn (module Syscall: Bindings))

let _ = syscall_test

let suite =
  "Syscall" >::: [
  ]
