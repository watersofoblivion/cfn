open Format
open Str

(* Regular expressions *)

let solo_re pattern =
  let pattern = sprintf "^%s$" pattern in
  regexp pattern

let num_pattern = "0\\|[1-9][0-9]*"
let num_re = solo_re num_pattern

let segment_pattern = "[1-9][0-9]*\\|[0-9a-zA-Z-]*[a-zA-Z-][0-9a-zA-Z-]*"
let segment_re = solo_re segment_pattern

let semver_re =
  let ver_pattern = sprintf "\\(%s\\)\\.\\(%s\\)\\.\\(%s\\)" num_pattern num_pattern num_pattern in
  let meta_pattern = sprintf "\\(%s\\(\\.%s\\)*\\)" segment_pattern segment_pattern in
  let semver_pattern = sprintf "^%s\\(-%s\\)?\\(\\+%s\\)?$" ver_pattern meta_pattern meta_pattern in
  regexp semver_pattern

let major_group = 1
let minor_group = 2
let patch_group = 3
let pre_release_group = 5
let build_info_group = 8

(* Metadata segments *)

type segment = Numeric of int | Alpha of string

let parse_segment segment =
  if string_match num_re segment 0
  then
    let str = matched_string segment in
    let num = int_of_string str in
    Numeric num
  else
    Alpha segment

(* Semantic Versions *)

type t = {
  major:       int;
  minor:       int;
  patch:       int;
  pre_release: string list;
  build_info:  string list
}

let semver major minor patch pre_release build_info =
  let validate_ver name v =
    if v < 0
    then
      let msg = sprintf "%d is not a valid %s version" v name in
      let exn = Invalid_argument msg in
      raise exn
    else v
  in
  let validate_meta name segments =
    let validate segment =
      if not (string_match segment_re segment 0)
      then
        let msg = sprintf "%S is not a valid %s segment" segment name in
        let exn = Invalid_argument msg in
        raise exn
    in
    let _ = List.iter validate segments in
    segments
  in
  { major       = validate_ver "major" major;
    minor       = validate_ver "minor" minor;
    patch       = validate_ver "patch" patch;
    pre_release = validate_meta "pre-release" pre_release;
    build_info  = validate_meta "build info" build_info }

(* Parsing *)

let of_string str =
  let extract_ver group =
    let ver_str = matched_group group str in
    int_of_string ver_str
  in
  let extract_meta group =
    try
      let meta = matched_group group str in
      String.split_on_char '.' meta
    with Not_found -> []
  in
  if string_match semver_re str 0
  then
    { major       = extract_ver major_group;
      minor       = extract_ver minor_group;
      patch       = extract_ver patch_group;
      pre_release = extract_meta pre_release_group;
      build_info  = extract_meta build_info_group }
  else
    let msg = sprintf "%S is not a valid semantic version" str in
    let exn = Invalid_argument msg in
    raise exn

(* Comparison *)

let rec compare_pre_release pre_release pre_release' =
  match pre_release, pre_release' with
    | [], [] -> 0
    | [], _ -> 1
    | _, [] -> -1
    | segment :: pre_release, segment' :: pre_release' ->
      let cmp = match parse_segment segment, parse_segment segment' with
        | Numeric num, Numeric num' -> compare num num'
        | Numeric _, Alpha _ -> 1
        | Alpha _, Numeric _ -> -1
        | Alpha str, Alpha str' -> compare str str'
      in
      if cmp <> 0
      then cmp
      else compare_pre_release pre_release pre_release'

let compare semver semver' =
  let major = compare semver.major semver'.major in
  if major <> 0
  then major
  else let minor = compare semver.minor semver'.minor in
       if minor <> 0
       then minor
       else let patch = compare semver.patch semver'.patch in
            if patch <> 0
            then patch
            else compare_pre_release semver.pre_release semver'.pre_release

(* Printing *)

let format_meta fmt prefix = function
  | [] -> ()
  | segments ->
    fprintf fmt "%s" prefix;
    let meta = String.concat "." segments in
    fprintf fmt "%s" meta

let format fmt semver =
  fprintf fmt "%d.%d.%d" semver.major semver.minor semver.patch;
  format_meta fmt "-" semver.pre_release;
  format_meta fmt "+" semver.build_info

let to_string semver =
  format str_formatter semver;
  flush_str_formatter ()

(* Compatibility Groups *)

type cg = int * t list

module Groups = Map.Make (struct type t = int let compare = Stdlib.compare end)
let compatibility_groups semvers =
  let fn groups semver =
    let add_version = function
      | None -> Some [semver]
      | Some versions -> Some (semver :: versions)
    in
    Groups.update semver.major add_version groups
  in
  let groups = List.fold_left fn Groups.empty semvers in
  let fn k v acc = (k, v) :: acc in
  Groups.fold fn groups []

let major (major, _) = major
let versions (_, versions) = versions

let min =
  { major       = -1;
    minor       = -1;
    patch       = -1;
    pre_release = [];
    build_info  = [] }

let newest (_, versions) =
  let fn acc version =
    if compare version acc > 0
    then version
    else acc
  in
  List.fold_left fn min versions
