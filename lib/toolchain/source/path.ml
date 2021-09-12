open Format
open Str

(* Version Control Systems *)

type vcs =
  | Git

let vcs_of_ext = function
  | ".git" -> Some Git
  | _ -> None

let vcs_ext = function
  | Git -> ".git"

(* Common *)

let legal_chars = "[^]@!/\"#$%&'()*,:;<=>?^[`{|}]+"

(* Names *)

type id = string

let id_pattern =
  sprintf "\\(%s\\(/\\|\\(/%s\\)*\\)\\)"
    legal_chars
    legal_chars
let id_re =
  let id_pattern = sprintf "^%s$" id_pattern in
  regexp id_pattern

let id str =
  let invalid str =
    let msg = sprintf "%S is not a valid project id" str in
    let exn = Invalid_argument msg in
    raise exn
  in

  let strip_vcs_ext uri = match Uri.path uri with
    | "" -> uri
    | path ->
      match String.sub path 1 (String.length path - 1) |> vcs_of_ext with
        | Some _ -> Uri.with_path uri ""
        | None ->
          match path |> Filename.extension |> vcs_of_ext with
            | Some _ ->
                path
                  |> Filename.remove_extension
                  |> Uri.with_path uri
            | None -> uri
  in

  if string_match id_re str 0
  then
    let uri =
      str
        |> sprintf "//%s"
        |> Uri.of_string
        |> strip_vcs_ext
    in
    let host = match Uri.host uri with
      | Some host -> host
      | None -> ""
    in
    let path = Uri.path uri in
    sprintf "%s%s" host path
  else invalid str

let name id = id

(* Project Paths *)

type project =
  | Internal
  | External of Uri.t * vcs * int

exception InternalProject

let project_re =
  let path_pattern = sprintf "^\\(\\.\\|%s@v\\([0-9]+\\)\\)$" id_pattern in
  regexp path_pattern

let path_group = 1
let uri_group = 2
let major_group = 5

let project str =
  let invalid str =
    let msg = sprintf "%S is not a valid project path" str in
    let exn = Invalid_argument msg in
    raise exn
  in

  if string_match project_re str 0
  then
    match matched_group path_group str with
      | "." -> Internal
      | _ ->
        let uri =
          str
            |> matched_group uri_group
            |> sprintf "//%s"
            |> Uri.of_string
        in
        let (uri, vcs) =
          match Uri.path uri with
            | "" -> (uri, Git)
            | path ->
              match String.sub path 1 (String.length path - 1) |> vcs_of_ext with
                | Some vcs ->
                  let uri = Uri.with_path uri "" in
                  (uri, vcs)
                | None ->
                  match path |> Filename.extension |> vcs_of_ext with
                    | Some vcs ->
                      let uri =
                        path
                          |> Filename.remove_extension
                          |> Uri.with_path uri
                      in
                      (uri, vcs)
                    | None -> (uri, Git)
        in
        let major =
          str
            |> matched_group major_group
            |> int_of_string
        in
        External(uri, vcs, major)
  else invalid str

let current = function
  | Internal -> true
  | _ -> false

let source = function
  | Internal -> "."
  | External(uri, _, _) ->
    let host = match Uri.host uri with
      | Some host -> host
      | None -> ""
    in
    let path = match Uri.path uri with
      | "" -> "/"
      | path -> path
    in
    String.concat "" [host; path]

let vcs = function
  | Internal -> raise InternalProject
  | External(_, vcs, _) -> vcs

let major = function
  | Internal -> raise InternalProject
  | External(_, _, major) -> major

let compare_project prj prj' =
  match (prj, prj') with
    | Internal, Internal -> 0
    | Internal, External _ -> -1
    | External _, Internal -> 1
    | _ ->
      match Stdlib.compare (source prj) (source prj') with
        | 0 -> Stdlib.compare (major prj) (major prj')
        | res -> res

(* Package Paths *)

type package = string

let package_pattern =
  sprintf "^\\(\\|\\(%s\\(/%s\\)*\\)\\)$"
    legal_chars
    legal_chars
let package_re =
  regexp package_pattern

let package str =
  let invalid str =
    let msg = sprintf "%S is not a valid package path" str in
    let exn = Invalid_argument msg in
    raise exn
  in

  let rec parse acc segments = match segments with
    | [] -> acc
    | "." :: segments -> parse acc segments
    | ".." :: segments ->
      let acc = Filename.dirname acc in
      parse acc segments
    | segment :: segments ->
      let acc = Filename.concat acc segment in
      parse acc segments
  in

  if string_match package_re str 0
  then
    str
      |> String.split_on_char '/'
      |> parse ""
  else
    invalid str

let path pkg = pkg
let compare_package = Stdlib.compare

(* Import Paths *)

type import = {
  project:   project;
  package:   package;
  recursive: bool;
}

let import str =
  match String.rindex_opt str ':' with
    | Some idx ->
      let prj = String.sub str 0 idx in
      let (pkg, recur) =
        let str = String.sub str (idx + 1) (String.length str - idx - 1) in
        let (pkg, last) = match String.rindex_opt str '/' with
          | None -> ("", str)
          | Some idx ->
            let pkg = String.sub str 0 idx in
            let last = String.sub str (idx + 1) (String.length str - idx - 1) in
            (pkg, last)
        in
        match last with
          | "..." -> (pkg, true)
          | _ ->
            let pkg = Filename.concat pkg last in
            (pkg, false)
      in
      { project   = project prj;
        package   = package pkg;
        recursive = recur }
    | None ->
      let msg = sprintf "%S is not a valid import path" str in
      let exn = Invalid_argument msg in
      raise exn

let recursive impt = impt.recursive
let prj impt = impt.project
let pkg impt = impt.package
