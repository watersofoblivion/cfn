open Runtime

open OUnit2

module type Bindings = sig
end

module Bind (Asm: Json.Asm) (Exe: TargetTest.Exe) = struct
end

let json_test tester ctxt =
  let ctx = Llvm.create_context () in
  let finally _ = Llvm.dispose_context ctx in
  let fn _ =
    let module Target = struct
      module Names = struct
        let prefix = "cfn++"
      end
      let ctx = ctx
      let md = Llvm.create_module ctx "test-module"
    end in

    let module Asm = Json.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Json = Bind (Asm) (Exe) in
    tester (module Json: Bindings) ctxt
  in
  Fun.protect ~finally fn

let _ = json_test

let suite =
  "JSON" >::: [
  ]
