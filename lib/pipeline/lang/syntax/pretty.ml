open Format

open Common

(* Types *)

let pp_ty fmt = function
  | Type.TyConstr constr -> Sym.pp_id fmt constr.id

(* Expressions *)

let pp_expr fmt = function
  | Ast.Bool expr -> fprintf fmt "%B" expr.value
  | Ast.Int expr -> fprintf fmt "%s" expr.lexeme
  | Ast.Long expr -> fprintf fmt "%s" expr.lexeme
  | Ast.Float expr -> fprintf fmt "%s" expr.lexeme
  | Ast.Double expr -> fprintf fmt "%s" expr.lexeme
  | Ast.Rune expr ->
    expr.value
      |> Uchar.to_char
      |> fprintf fmt "'%c'"
  | Ast.String expr -> fprintf fmt "%S" expr.value
  | Ast.Ident expr -> Sym.pp_id fmt expr.id

(* Patterns *)

let pp_patt fmt = function
  | Ast.PattGround _ -> Pretty.ground fmt
  | Ast.PattVar patt -> Sym.pp_id fmt patt.id

(* Bindings *)

let pp_binding fmt = function
  | Ast.ValueBinding binding ->
    let pp_ty = pp_print_option (fun fmt t -> fprintf fmt ": %a" pp_ty t) in
    fprintf fmt "%a%a = %a" pp_patt binding.patt pp_ty binding.ty pp_expr binding.value

(* Top-Level Expressions *)

let pp_top fmt = function
  | Ast.Let top -> fprintf fmt "let %a" pp_binding top.binding
  | Ast.Val top -> fprintf fmt "val %a" pp_binding top.binding

(* Imports *)

let pp_name fmt = function
  | Ast.Name name -> fprintf fmt "%a" Sym.pp_id name.id

let pp_src fmt = function
  | Ast.Source src -> fprintf fmt "%a" pp_name src.name

let pp_from fmt = function
  | Ast.From from -> fprintf fmt "@[from@ %a@ @]" pp_src from.src

let pp_alias fmt = function
  | Ast.Alias alias ->
    fprintf fmt "@[%a" pp_name alias.pkg;
    let _ =
      match alias.alias with
        | Some alias -> fprintf fmt "@[@ ->@ %a@]" pp_name alias
        | None -> ()
    in
    fprintf fmt "@]"

let pp_pkgs fmt = function
  | Ast.Packages pkgs ->
    fprintf fmt "@[<v>";
    let pp_sep fmt _ = fprintf fmt "@ | " in
    pp_sep fmt ();
    pp_print_list ~pp_sep pp_alias fmt pkgs.pkgs;
    fprintf fmt "@]"

let pp_import fmt = function
  | Ast.Import stmt ->
    fprintf fmt "@[";
    let _ = match stmt.from with
      | Some clause -> pp_from fmt clause
      | None -> ()
    in
    fprintf fmt "import";
    pp_pkgs fmt stmt.pkgs;
    fprintf fmt "@]"

(* Package Statment *)

let pp_pkg fmt = function
  | Ast.Package pkg -> fprintf fmt "package %a" pp_name pkg.id

(* Files *)

let pp_file fmt = function
  | Ast.File file ->
    fprintf fmt "@[<v>%a" pp_pkg file.pkg;
    let _ = match file.imports with
      | [] -> ()
      | _ ->
        fprintf fmt "@ @ ";
        let pp_sep fmt _ = fprintf fmt "@ " in
        pp_print_list ~pp_sep pp_import fmt file.imports;
    in
    fprintf fmt "@]"
