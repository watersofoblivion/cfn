open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  val exit : unit -> unit
end

module Bind (Syscall: Syscall.Asm) (Exe: TargetTest.Exe) = struct
  let exit =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Syscall.Names.exit
end

let syscall_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in

    let module Asm = Syscall.Generate (Types) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Syscall = Bind (Asm) (Exe) in
    test_fn (module Syscall: Bindings))

let _ = syscall_test

let suite =
  "Syscall" >::: [
  ]
