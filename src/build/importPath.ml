type t = {
  import_path: string;
  recursive:   bool;
  relative:    bool;
  github:      bool;
  owner:       string;
  repo:        string
}

let of_string str =
  let _ = str in
  { import_path = "";
    recursive   = false;
    relative    = false;
    github      = false;
    owner       = "";
    repo        = "" }
