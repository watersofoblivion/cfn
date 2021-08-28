open Runtime

open OUnit2

module type Bindings = sig
end

module Bind (Http: Http.Asm) (Exe: TargetTest.Exe) = struct
end

let http_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Asm = Http.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Http = Bind (Asm) (Exe) in
    test_fn (module Http: Bindings))

let _ = http_test

let suite =
  "HTTP" >::: [
  ]
