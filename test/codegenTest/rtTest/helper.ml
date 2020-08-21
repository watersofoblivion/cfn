open Format

open OUnit2

let init _ =
  if Llvm_executionengine.initialize ()
  then ()
  else assert_failure "Could not initialize LLVM JIT"

let get_global_var ty name = Llvm_executionengine.get_global_value_address name ty
let get_function ty name = Llvm_executionengine.get_function_address name ty

let get_global_uint64_var = get_global_var Ctypes.uint64_t

let llvm_test setup tester ctxt =
  let ctx = Llvm.create_context () in
  let finally _ = Llvm.dispose_context ctx in

  let md = Llvm.create_module ctx "test-module" in
  setup md;
  let _ = match Llvm_analysis.verify_module md with
    | None -> ()
    | Some msg ->
      Llvm.dump_module md;
      let msg = sprintf "Invalid module: %s\n%!" msg in
      assert_failure msg
  in
  let ee = Llvm_executionengine.create md in
  Fun.protect ~finally (fun _ -> tester ee ctxt)
