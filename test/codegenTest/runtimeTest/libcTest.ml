open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  val malloc : nativeint -> unit Ctypes.ptr
end

module Bind (Libc: Libc.Asm) (Exe: TargetTest.Exe) = struct
  let malloc =
    let ty = Foreign.funptr (nativeint @-> returning (ptr void)) in
    Exe.func ty Libc.Names.malloc
end

let libc_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in
    let module Asm = Libc.Generate (Types) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Libc = Bind (Asm) (Exe) in
    test_fn (module Libc: Bindings))

let _ = libc_test

let suite =
  "Libc" >::: [
  ]
