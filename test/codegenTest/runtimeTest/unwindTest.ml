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

let unwind_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Types = Types.Generate (Target) in

    let module Asm = Unwind.Generate (Types) (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Unwind = Bind (Asm) (Exe) in
    test_fn (module Unwind: Bindings))

let _ = unwind_test

let suite =
  "Unwind" >::: [
  ]
