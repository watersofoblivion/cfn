open Runtime

open OUnit2

module type Bindings = sig
end

module Bind (Asm: Dwarf.Asm) (Exe: TargetTest.Exe) = struct
end

let dwarf_test test_fn =
  TargetTest.test (fun (module Target: Target.Asm) ->
    let module Asm = Dwarf.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Dwarf = Bind (Asm) (Exe) in
    test_fn (module Dwarf: Bindings))

let _ = dwarf_test

let suite =
  "DWARF" >::: [
  ]
