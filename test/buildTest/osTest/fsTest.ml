open OUnit2

let test_dir =
  let test_dir ctxt = () in
  let test_temp_dir ctxt = () in
  let test_in_dir ctxt = () in
  let test_in_temp_dir ctxt = () in
  "Directories" >::: [
    test_dir;
    test_temp_dir;
    test_in_dir;
    test_in_temp_dir
  ]

let test_file =
  let test_file ctxt = () in
  let test_ext ctxt = () in
  let test_with_ext ctxt = () in
  let test_replace_ext ctxt = () in
  "Files" >::: [
    test_file;
    test_ext;
    test_with_ext;
    test_replace_ext
  ]

let test_path =
  let test_path ctxt = () in
  let test_basename ctxt = () in
  let test_filename ctxt = () in
  "Paths" >::: [
    test_path;
    test_basename;
    test_filename
  ]

let suite =
  "Filesystem" >::: [
    test_dir;
    test_file;
    test_path
  ]
