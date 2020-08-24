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

let libc_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Asm = Libc.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Libc = Bind (Asm) (Exe) in
    test_fn (module Libc: Bindings))

let _ = libc_test

let suite =
  "Libc" >::: [
  ]
