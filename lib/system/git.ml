(* Repositories *)

type r = {
  root:           string;
  default_branch: string;
}

let repo root default_branch =
  { root           = root;
    default_branch = default_branch }
let root repo = repo.root
let default_branch repo = repo.default_branch

(* Commands *)

let git repo args handler =
  let fn _ = Os.run "git" args handler in
  Os.in_dir repo.root fn handler

let clone uri dir =
  let uri_str = Uri.to_string uri in
  let repo = repo dir "" in

  let _ = Os.run "git" ["clone"; uri_str; dir] Os.ignore in
  let default_branch = git repo ["symbolic-ref"; "HEAD"] Os.line in
  { repo with default_branch = default_branch }

let fetch repo = git repo ["fetch"; "--all"] Os.ignore
let checkout repo gitref = git repo ["checkout"; gitref] Os.ignore

let timestamp repo gitref =
  git repo ["show"; "--format=%ad"; "--date=format:%Y%m%d%H%M%S"; gitref] Os.first_line

let for_each_ref repo refs =
  let args = ["for-each-ref"; "--color=never"; "--format=%(refname:short) %(objectname)"] @ refs in
  let map line = match String.split_on_char ' ' line with
    | refname :: sha :: [] -> (refname, sha)
    | _ -> failwith "Unpossible!"
  in
  git repo args Os.lines
    |> List.map map

let has_prefix prefix str =
  let prefix_len = String.length prefix in
  String.length str >= prefix_len && String.sub str 0 prefix_len = prefix

let trim_prefix prefix str =
  let prefix_len = String.length prefix in
  if has_prefix prefix str
  then String.sub str prefix_len (String.length str - prefix_len)
  else str

let tags repo =
  let filter_map (tag, sha) =
    if has_prefix "v" tag
    then
      try
        let version =
          tag
            |> trim_prefix "v"
            |> Semver.of_string
        in
        Some (version, sha)
      with Invalid_argument _ -> None
    else None
  in
  for_each_ref repo ["refs/tags"]
    |> List.filter_map filter_map

let branches repo =
  let remote_branch_prefixes =
    let map name = name ^ "/" in
    git repo ["remote"] Os.lines
      |> List.map map
  in
  let filter_map (branch, sha) =
    let short_branch =
      let find branch_prefix = has_prefix branch_prefix branch in
      match List.find_opt find remote_branch_prefixes with
        | Some prefix -> trim_prefix prefix branch
        | None -> branch
    in
    if has_prefix "v" short_branch
    then
      match short_branch |> trim_prefix "v" |> int_of_string_opt with
        | None -> None
        | Some major ->
          let timestamp = timestamp repo branch in
          let version = Semver.semver major 0 0 [timestamp; sha] [] in
          Some (version, sha)
    else None
  in
  for_each_ref repo ["refs/heads"; "refs/remotes"]
    |> List.filter_map filter_map

let versions repo =
  let fold versions fn =
    let fold versions (semver, sha) = Semver.add semver sha versions in
    repo
      |> fn
      |> List.fold_left fold versions
  in
  let versions =
    [tags; branches]
      |> List.fold_left fold Semver.empty
  in
  try
    let _ = Semver.latest_prerelease 0 versions in
    versions
  with Not_found ->
    let sha = git repo ["rev-parse"; repo.default_branch] Os.line in
    let timestamp = timestamp repo sha in
    let semver = Semver.semver 0 0 0 [timestamp; sha] [] in
    Semver.add semver sha versions
