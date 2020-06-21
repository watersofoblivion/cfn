open Format

type src = {
  path:        string;
  mtime:       float;
  cfn_files:   string list;
  other_files: string list;
}

let src_of_path path =
  let fn src file =
    let path = Filename.concat path file in
    let mtime =
      let stat = Unix.stat path in
      max stat.st_mtime src.mtime
    in
    match Filename.extension file with
      | ".cfn" ->
        { src with mtime     = mtime;
                   cfn_files = file :: src.cfn_files }
      | _ ->
        { src with mtime       = mtime;
                   other_files = file :: src.other_files }
  in
  let src =
    { path        = path;
      mtime       = 0.0;
      cfn_files   = [];
      other_files = [] }
  in
  let dir = Sys.readdir path in
  Array.fold_left fn src dir

type lib = {
  path:           string;
  mtime:          float;
  bc_file:        string;
  template_files: string list;
  other_files:    string list;
}

let lib_of_path path =
  let fn lib file =
    let path = Filename.concat path file in
    let mtime =
      let stat = Unix.stat path in
      max stat.st_mtime lib.mtime
    in
    match Filename.extension file with
      | ".yaml" ->
        { lib with mtime          = mtime;
                   template_files = file :: lib.template_files }
      | ".bc" ->
        if lib.bc_file != ""
        then
          let msg = sprintf "multiple lib files in %s: %s and %s" path lib.bc_file file in
          failwith msg
        else
          { lib with mtime   = mtime;
                     bc_file = file }
      | _ ->
          { lib with mtime       = mtime;
                     other_files = file :: lib.other_files }
  in
  let lib =
    { path           = path;
      mtime          = 0.0;
      bc_file        = "";
      template_files = [];
      other_files    = [] }
  in
  let dir = Sys.readdir path in
  Array.fold_left fn lib dir


type t = {
  package_name: string;
  source:       src;
  compiled:     lib;
}

let create ctx prj path =
  let (_, _, _) = ctx, prj, path in
  let _ = src_of_path "" in
  let _ = lib_of_path "" in
  { package_name = "";
    source       = { path = ""; mtime = 0.0; cfn_files = []; other_files = [] };
    compiled     = { path = ""; mtime = 0.0; bc_file = ""; template_files = []; other_files = [] } }

let scan pkg = pkg
let imports _ = ()
let parse _ = ()

let path _ = ""
let name _ = ""
let src _ = { path = ""; mtime = 0.0; cfn_files = []; other_files = [] }
let lib _ = { path = ""; mtime = 0.0; bc_file = ""; template_files = []; other_files = [] }
let up_to_date _ = false
