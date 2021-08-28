open Runtime

open OUnit2

module type Bindings = sig
end

module Bind (Json: Json.Asm) (Exe: TargetTest.Exe) = struct
end

let json_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Asm = Json.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Json = Bind (Asm) (Exe) in
    test_fn (module Json: Bindings))

let _ = json_test

let suite =
  "JSON" >::: [
  ]
