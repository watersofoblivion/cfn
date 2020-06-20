type pkg = {
  package_path:     string;
  package_name:     string;
  src_path:         string;
  cfn_files:        string list;
  other_files:      string list;
  last_updated:     float
}

let pkg ctx import_path package_path =
  let (_, _, _) = ctx, import_path, package_path in
  { package_path = "";
    package_name = "";
    src_path     = "";
    cfn_files    = [];
    other_files  = [];
    last_updated = 0.0 }

let pkg_imports ctx import_path package_path =
  let (_, _, _) = ctx, import_path, package_path in
  []
