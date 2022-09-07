(* Source Files *)

open Format

type file = File of { pkg: Pkg.pkg; imports: Import.import list; tops: Top.top list }

let pp_file fmt file =
  let pp_prefix fmt =
    pp_print_space fmt ();
    pp_print_space fmt ()
  in
  let pp_list pp = function
    | [] -> ()
    | lst -> fprintf fmt "%t%a" pp_prefix (pp_print_list ~pp_sep:pp_print_space pp) lst
  in
  match file with
    | File file ->
      pp_open_vbox fmt 0;
      Pkg.pp_pkg fmt file.pkg;
      pp_list Import.pp_import file.imports;
      pp_list Top.pp_top file.tops;
      pp_close_box fmt ()
