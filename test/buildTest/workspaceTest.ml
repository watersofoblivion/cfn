(* open Format *)

open OUnit2

(* open Build *)

(* Test Initialization *)

let _ =
  try
    Sys.getenv "CFN_HOME"
  with Not_found ->
    Unix.putenv "CFN_HOME" "";
    ""

(* let cwd = Sys.getcwd () *)

(* Test Initialization *)
(* let test_test_init =
  let test_cfn_root _ =
    try
      let _ = Sys.getenv "CFN_HOME" in
      ()
    with Not_found ->
      assert_failure "${CFN_HOME} not set.  Tests will fail."
  in
  "Test Initialization" >::: [
    "${CFN_HOME} Environment Variable" >:: test_cfn_root
  ] *)

(* Workspaces *)

let suite =
  "Workspaces" >::: [
  ]
