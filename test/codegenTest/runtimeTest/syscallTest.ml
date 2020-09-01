open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  module Types : TypesTest.Bindings

  val exit : Types.void_t -> Types.void_t
end

module Bind (Types: TypesTest.Bindings) (Syscall: Syscall.Asm) (Exe: TargetTest.Exe) = struct
  module Types = Types

  let exit =
    let ty = Foreign.funptr (void @-> returning void) in
    Exe.func ty Syscall.Names.exit
end

let syscall_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in

    let module Asm = Syscall.Generate (Types) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Types = TypesTest.Bind (Types) (Exe) in
    let module Syscall = Bind (Types) (Asm) (Exe) in
    test_fn (module Syscall: Bindings))

let _ = syscall_test

let suite =
  "Syscall" >::: [
  ]
