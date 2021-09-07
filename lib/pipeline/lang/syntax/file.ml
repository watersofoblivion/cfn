(* Source Files *)

open Format

(* Syntax *)

type file = File of { pkg: Import.pkg; imports: Import.import list; tops: Ast.top list }

(* Constructors *)

let file pkg imports tops = File { pkg; imports; tops }

(* Pretty Printing *)

let pp_file fmt = function
  | File file ->
    fprintf fmt "@[<v>%a" Import.pp_pkg file.pkg;
    let _ = match file.imports with
      | [] -> ()
      | _ ->
        fprintf fmt "@ @ ";
        let pp_sep fmt _ = fprintf fmt "@ " in
        pp_print_list ~pp_sep Import.pp_import fmt file.imports;
    in
    fprintf fmt "@]"
