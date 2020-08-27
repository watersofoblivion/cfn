open Runtime

open OUnit2

module type Bindings = sig
end

module Bind (Asm: Types.Asm) (Exe: TargetTest.Exe) = struct
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
