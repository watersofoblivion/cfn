type dependency = {
  import_path: string;
  version:     string;
  direct:      bool
}

type prj = {
  import_path:  string;
  dependencies: dependency list
}
