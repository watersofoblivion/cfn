open Format
open Str

(* Sources *)

let assert_valid_major major =
  if major < 0
  then
    let exn = Invalid_argument "major version cannot be negative" in
    raise exn

type src =
  | Internal
  | GitHub of string * string * int
  | External of string option * string * int option * string option * int

(* Regular expressions *)

let host_pattern = "\\([^@:/\\.]+\\.\\)+[a-z]+"

let src_pattern =
  let segment_pattern = "[^/%@]+" in
  let internal_pattern = "\\." in
  let external_pattern =
    let github_pattern = sprintf "\\(github\\.com/\\(%s\\)/\\(%s\\)\\(/[^%@]*\\)?\\)" segment_pattern segment_pattern in
    let external_pattern = sprintf "\\(\\(\\([^:]+\\)://\\)?\\(%s\\)\\(:\\([0-9]+\\)\\)?\\(\\(/%s\\)*\\)\\)" host_pattern segment_pattern in
    sprintf "\\(%s\\|%s\\)@v\\([0-9]+\\)" github_pattern external_pattern
  in
  sprintf "\\(%s\\|%s\\)" internal_pattern external_pattern

let source_group = 1
let owner_group = 4
let repo_group = 5
let extra_group = 6
let proto_group = 9
let host_group = 10
let port_group = 13
let path_group = 14
let major_group = 16

(* Constructors *)

let internal_src = Internal

let github_src owner repo major =
  let invalid s =
    let msg = sprintf "GitHub %s cannot be blank" s in
    let exn = Invalid_argument msg in
    raise exn
  in
  assert_valid_major major;
  match (owner, repo) with
    | "", "" -> invalid "owner and repo"
    | "", _ -> invalid "owner"
    | _, "" -> invalid "repo"
    | _, _ -> GitHub(owner, repo, major)

let host_re = regexp host_pattern
let external_src proto host port path major =
  assert_valid_major major;
  match host with
    | "" ->
      let exn = Invalid_argument "host cannot be blank" in
      raise exn
    | _ ->
      if string_match host_re host 0
      then External(proto, host, port, path, major)
      else
        let msg = sprintf "%S is not a valid host" host in
        let exn = Invalid_argument msg in
        raise exn

let src_re = regexp src_pattern
let of_string s =
  let string_group grp = matched_group grp s in
  let opt_string grp =
    try Some (string_group grp)
    with Not_found -> None
  in
  let int_group grp = int_of_string (string_group grp) in
  let opt_int grp = match opt_string grp with
    | Some s -> Some (int_of_string s)
    | None -> None
  in
  let parse_github major =
    let owner = string_group owner_group in
    let repo = string_group repo_group in
    try
      let extra = string_group extra_group in
      let msg = sprintf "GitHub source contains extra path segments %S" extra in
      let exn = Invalid_argument msg in
      raise exn
    with Not_found -> github_src owner repo major
  in
  let parse_external major =
    let proto = opt_string proto_group in
    let host = string_group host_group in
    let port = opt_int port_group in
    let path = match string_group path_group with
      | "" -> None
      | path -> Some (String.sub path 1 (String.length path - 1))
    in
    external_src proto host port path major
  in
  if string_match src_re s 0
  then
    if string_group source_group = "."
    then internal_src
    else
      let major = int_group major_group in
      try parse_github major
      with Not_found -> parse_external major
  else
    let msg = sprintf "%S is not a valid package path" s in
    let exn = Invalid_argument msg in
    raise exn

let format fmt = function
  | Internal -> fprintf fmt "."
  | GitHub(owner, repo, major) -> fprintf fmt "github.com/%s/%s@v%d" owner repo major
  | External(proto, host, port, path, major) ->
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
    fprintf fmt "@v%d" major

let to_string src =
  format str_formatter src;
  flush_str_formatter ()

(* Projects *)

type t = unit

let create _ _ = ()
let src _ = Internal
let fetch prj = prj
let major _ = 0
let version _ = Semver.semver 0 0 0 [] []
let tagged_versions _ = []
let major_branches _ = []
let pkg_names _ = []
let path _ = ""
