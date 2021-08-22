open Ctypes

open Runtime

open OUnit2

module type Bindings = sig
  val int_to_ptr : nativeint -> unit Ctypes.ptr
  val ptr_to_int : unit Ctypes.ptr -> nativeint
end

module Bind (Types: Types.Asm) (Exe: TargetTest.Exe) = struct
  let int_to_ptr = ptr_of_raw_address
  let ptr_to_int = raw_address_of_ptr
end

let types_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Asm = Types.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Types = Bind (Asm) (Exe) in
    test_fn (module Types: Bindings))

let _ = types_test

let suite =
  "Core Types" >::: [
  ]
