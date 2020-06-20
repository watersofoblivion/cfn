open Format
open Str

type src =
  | Internal
  | GitHub of string * string * int
  | External of string option * string * int option * string option * int

let internal = Internal

let github owner repo ver =
  let invalid s =
    let msg = sprintf "GitHub %s cannot be blank" s in
    let exn = Invalid_argument msg in
    raise exn
  in
  if ver < 0
  then
    let exn = Invalid_argument "major version cannot be negative" in
    raise exn
  else
    match (owner, repo) with
      | "", "" -> invalid "owner and repo"
      | "", _ -> invalid "owner"
      | _, "" -> invalid "repo"
      | _, _ -> GitHub(owner, repo, ver)

let host_pattern = "\\([^@:/\\.]+\\.\\)+[a-z]+"
let host_re = regexp host_pattern

let ext proto host port path ver =
  if ver < 0
  then
    let exn = Invalid_argument "major version cannot be negative" in
    raise exn
  else
    match host with
      | "" ->
        let exn = Invalid_argument "host cannot be blank" in
        raise exn
      | _ ->
        if string_match host_re host 0
        then External(proto, host, port, path, ver)
        else
          let msg = sprintf "%S is not a valid host" host in
          let exn = Invalid_argument msg in
          raise exn

type t = {
  source:    src;
  package:   string;
  recursive: bool;
}

let create src pkg recur =
  { source    = src;
    package   = pkg;
    recursive = recur }

let path_re =
  let segment_pattern = "[^/%@]+" in

  let source_pattern =
    let internal_pattern = "\\." in
    let external_pattern =
      let github_pattern = sprintf "\\(github\\.com/\\(%s\\)/\\(%s\\)\\(/[^%@]*\\)?\\)" segment_pattern segment_pattern in
      let external_pattern = sprintf "\\(\\(\\([^:]+\\)://\\)?\\(%s\\)\\(:\\([0-9]+\\)\\)?\\(\\(/%s\\)*\\)\\)" host_pattern segment_pattern in
      sprintf "\\(%s\\|%s\\)@v\\([0-9]+\\)" github_pattern external_pattern
    in
    sprintf "\\(%s\\|%s\\)" internal_pattern external_pattern
  in

  let package_pattern = sprintf "\\(\\|\\(\\(\\(%s/\\)*\\)\\(\\.\\.\\.\\|%s\\)\\)\\)" segment_pattern segment_pattern in
  let path_pattern = sprintf "^%s:%s$" source_pattern package_pattern in

  regexp path_pattern

(* let fn path n =
  let _ = string_match path_re path 0 in
  let rec fn = function
    | 0 -> ()
    | n ->
      fn (n - 1);
      printf "Group %d: %s\n"
        n
        (try
          let s = matched_group n path in
          sprintf "%S" s
        with Not_found -> "n/a");
  in
  fn n *)

let source_group = 1
let owner_group = 4
let repo_group = 5
let extra_group = 6
let proto_group = 9
let host_group = 10
let port_group = 13
let path_group = 14
let version_group = 16
let pkg_group = 17
let pkg_path_group = 19
let rec_group = 21

let of_string str =
  let string_group grp = matched_group grp str in
  let opt_string grp =
    try Some (string_group grp)
    with Not_found -> None
  in
  let int_group grp = int_of_string (string_group grp) in
  let opt_int grp = match opt_string grp with
    | Some s -> Some (int_of_string s)
    | None -> None
  in

  if string_match path_re str 0
  then
    let src =
      if string_group source_group = "."
      then internal
      else
        let version = int_group version_group in

        try
          let owner = string_group owner_group in
          let repo = string_group repo_group in
          let _ =
            try
              let extra = string_group extra_group in
              let msg = sprintf "GitHub source contains extra path segments %S" extra in
              let exn = Invalid_argument msg in
              raise exn
            with Not_found -> ()
          in
          github owner repo version
        with Not_found ->
          let proto = opt_string proto_group in
          let host = string_group host_group in
          let port = opt_int port_group in
          let path = match string_group path_group with
            | "" -> None
            | path -> Some (String.sub path 1 (String.length path - 1))
          in
          ext proto host port path version
    in
    let (pkg, recur) =
      try begin
        match string_group pkg_group with
          | "" -> ("", false)
          | _ ->
            begin
              let pkg_path =
                try string_group pkg_path_group
                with Not_found -> ""
              in
              try begin
                match string_group rec_group with
                  | "..." -> (pkg_path, true)
                  | tl -> (pkg_path ^ tl, false)
              end
              with Invalid_argument _ -> (pkg_path, false)
            end
      end
      with Invalid_argument _ ->
        ("", false)
    in
    let pkg = match pkg with
      | "" -> pkg
      | pkg ->
        if pkg.[String.length pkg - 1] = '/'
        then String.sub pkg 0 (String.length pkg - 1)
        else pkg
    in
    create src pkg recur
  else
    let msg = sprintf "%S is not a valid package path" str in
    let exn = Invalid_argument msg in
    raise exn

let format fmt path =
    let _ = match path.source with
    | Internal -> fprintf fmt "."
    | GitHub(owner, repo, ver) -> fprintf fmt "github.com/%s/%s@v%d" owner repo ver
    | External(proto, host, port, path, ver) ->
      let _ = match proto with
        | Some proto -> fprintf fmt "%s://" proto
        | None -> ()
      in
      let _ = fprintf fmt "%s" host in
      let _ = match port with
        | Some port -> fprintf fmt ":%d" port
        | None -> ()
      in
      let _ = match path with
        | Some path -> fprintf fmt "/%s" path
        | None -> ()
      in
      fprintf fmt "@v%d" ver
  in
  let _ = fprintf fmt ":" in
  match (path.package, path.recursive) with
    | "", true -> fprintf fmt "..."
    | pkg, true -> fprintf fmt "%s/..." pkg
    | pkg, false -> fprintf fmt "%s" pkg

let to_string path =
  format str_formatter path;
  flush_str_formatter ()
