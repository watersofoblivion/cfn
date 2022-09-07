(* Source Files *)

open Format

(* Syntax *)

type file = File of { pkg: Pkg.pkg; imports: Import.import list; tops: Top.top list }

(* Constructors *)

let file pkg imports tops = File { pkg; imports; tops }

(* Pretty Printing *)

let pp_list pp sep fmt lst =
  let pp_sep fmt _ = fprintf fmt sep in
  let _ = match lst with
    | [] -> ()
    | _ -> fprintf fmt "@ @ "
  in
  pp_print_list ~pp_sep pp fmt lst

let pp_imports = pp_list Import.pp_import "@ "
let pp_tops = pp_list Top.pp_top "@ @ "

let pp_file fmt = function
  | File file ->
    fprintf fmt "@[<v>%a" Pkg.pp_pkg file.pkg;
    pp_imports fmt file.imports;
    pp_tops fmt file.tops;
    fprintf fmt "@]"
