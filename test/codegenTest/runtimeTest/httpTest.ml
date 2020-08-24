open Runtime

open OUnit2

module type Bindings = sig
end

module Bind (Asm: Http.Asm) (Exe: TargetTest.Exe) = struct
end

let http_test tester ctxt =
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

    let module Asm = Http.Generate (Target) in
    let module Exe = TargetTest.Compile (Target) in

    let module Http = Bind (Asm) (Exe) in
    tester (module Http: Bindings) ctxt
  in
  Fun.protect ~finally fn

let _ = http_test

let suite =
  "HTTP" >::: [
  ]
