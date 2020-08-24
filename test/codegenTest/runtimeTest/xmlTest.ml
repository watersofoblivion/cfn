open Runtime

open OUnit2

module type Bindings = sig
end

module Bind (Asm: Xml.Asm) (Exe: TargetTest.Exe) = struct
end

let xml_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Asm = Xml.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Xml = Bind (Asm) (Exe) in
    test_fn (module Xml: Bindings))

let _ = xml_test

let suite =
  "XML" >::: [
  ]
