type t = {
  name:     string;
  versions: (string * Version.t) list
}

let of_dir path =
  { name     = path;
    versions = [] }
