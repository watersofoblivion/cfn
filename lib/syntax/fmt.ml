open Format

open Common

(* Types *)

let ty fmt = function
  | Type.Constr constr -> Sym.pp_id fmt constr.id

(* Expressions *)

let expr fmt = function
  | Ast.Bool b -> fprintf fmt "%B" b.value
  | Ast.Int i -> fprintf fmt "%s" i.lexeme
  | Ast.Long l -> fprintf fmt "%s" l.lexeme
  | Ast.Float f -> fprintf fmt "%s" f.lexeme
  | Ast.Double d -> fprintf fmt "%s" d.lexeme
  | Ast.Rune r -> fprintf fmt "'%c'" (Uchar.to_char r.value)
  | Ast.String s ->
    s.value
      |> List.map Uchar.to_char
      |> List.map (sprintf "%c")
      |> String.concat ""
      |> fprintf fmt "%S"
  | Ast.Ident ident -> Sym.pp_id fmt ident.id

(* Patterns *)

let patt fmt = function
  | Ast.PattGround _ -> Pretty.ground fmt
  | Ast.PattVar patt -> Sym.pp_id fmt patt.id

(* Bindings *)

let binding fmt = function
  | Ast.ValueBinding binding ->
    let ty = pp_print_option (fun fmt t -> fprintf fmt ": %a" ty t) in
    fprintf fmt "%a%a = %a" patt binding.patt ty binding.ty expr binding.value

(* Top-Level Expressions *)

let top fmt = function
  | Ast.Let top -> fprintf fmt "let %a" binding top.binding
  | Ast.Val top -> fprintf fmt "val %a" binding top.binding

(* Imports *)

let name fmt = function
  | Ast.Name name -> fprintf fmt "%a" Sym.pp_id name.id

let src fmt = function
  | Ast.Source src -> fprintf fmt "%a" name src.name

let from fmt = function
  | Ast.From from -> fprintf fmt "@[from@ %a@ @]" src from.src

let alias fmt = function
  | Ast.Alias alias ->
    fprintf fmt "@[%a" name alias.pkg;
    let _ =
      match alias.alias with
        | Some alias -> fprintf fmt "@[@ ->@ %a@]" name alias
        | None -> ()
    in
    fprintf fmt "@]"

let pkgs fmt = function
  | Ast.Packages pkgs ->
    fprintf fmt "@[<v>";
    let pp_sep fmt _ = fprintf fmt "@ | " in
    pp_sep fmt ();
    pp_print_list ~pp_sep alias fmt pkgs.pkgs;
    fprintf fmt "@]"

let import fmt = function
  | Ast.Import stmt ->
    fprintf fmt "@[";
    let _ = match stmt.from with
      | Some clause -> from fmt clause
      | None -> ()
    in
    fprintf fmt "import";
    pkgs fmt stmt.pkgs;
    fprintf fmt "@]"

(* Package Statment *)

let pkg fmt = function
  | Ast.Package pkg -> fprintf fmt "package %a" name pkg.id

(* Files *)

let file fmt = function
  | Ast.File file ->
    fprintf fmt "@[<v>%a" pkg file.pkg;
    let _ = match file.imports with
      | [] -> ()
      | _ ->
        fprintf fmt "@ @ ";
        let pp_sep fmt _ = fprintf fmt "@ " in
        pp_print_list ~pp_sep import fmt file.imports;
    in
    fprintf fmt "@]"
