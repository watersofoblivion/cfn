(** {1 Parser} *)

open Common

(**
 * {2 Entry Points}
 *
 * Each takes a path to a source file [path] and parses the file in the
 * environment [env].  The parsed value and a (possibly updated) environment are
 * passed to the continuation [kontinue].
 *)

val parse_package_only : string -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.file -> 'a) -> 'a
(** [parse_package_only path env kontinue] parses the source file through the
    package statement only. *)

val parse_imports_only : string -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.file -> 'a) -> 'a
(** [parse_package_only path env kontinue] parses the source file through the
    package statement and import statements only. *)

val parse_file : string -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.file -> 'a) -> 'a
(** [parse_file path env kontinue] parses the source file. *)

(**
 * {2 Test Parsers}
 *
 * These parsers are exposed purely for unit testing and should not be used
 * externally.
 *
 * Each takes a lexing buffer [lexbuf] and parses it in the environment [env].
 * The parsed value and a (possibly updated) environment are passed to the
 * continuation [kontinue].
 *)

val parse_annot : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.ty -> 'a) -> 'a
(** [parse_annot lexbuf env kontinue] parses a type annotation. *)

val parse_ty : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.ty -> 'a) -> 'a
(** [parse_ty lexbuf env kontinue] parses a type constructor. *)

val parse_un : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.un -> 'a) -> 'a
(** [parse_un lexbuf env kontinue] parses a unary operator. *)

val parse_bin : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.bin -> 'a) -> 'a
(** [parse_bin lexbuf env kontinue] parses a binary operator. *)

val parse_patt : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.patt -> 'a) -> 'a
(** [parse_patt lexbuf env kontinue] parses a pattern. *)

val parse_lit : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.expr -> 'a) -> 'a
(** [parse_lit lexbuf env kontinue] parses a literal value. *)

val parse_ident : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.expr -> 'a) -> 'a
(** [parse_ident lexbuf env kontinue] parses an identifier. *)

val parse_atom : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.expr -> 'a) -> 'a
(** [parse_atom lexbuf env kontinue] parses an atomic expression. *)

val parse_expr : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.expr -> 'a) -> 'a
(** [parse_expr lexbuf env kontinue] parses an expression. *)

val parse_block : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.expr -> 'a) -> 'a
(** [parse_block lexbuf env kontinue] parses an block expression. *)

val parse_binding : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.binding -> 'a) -> 'a
(** [parse_binding lexbuf env kontinue] parses a binding. *)

val parse_top : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.top -> 'a) -> 'a
(** [parse_top lexbuf env kontinue] parses a top-level expression. *)

val parse_name : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.name -> 'a) -> 'a
(** [parse_name lexbuf env kontinue] parses an import name. *)

val parse_local : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.name -> 'a) -> 'a
(** [parse_local lexbuf env kontinue] parses a local alias name. *)

val parse_alias : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.alias -> 'a) -> 'a
(** [parse_alias lexbuf env kontinue] parses an alias clause. *)

val parse_pkgs : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.pkgs -> 'a) -> 'a
(** [parse_pkgs lexbuf env kontinue] parses an alias list. *)

val parse_src : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.src -> 'a) -> 'a
(** [parse_src lexbuf env kontinue] parses an import source. *)

val parse_from : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.from -> 'a) -> 'a
(** [parse_from lexbuf env kontinue] parses a from clause. *)

val parse_import : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.import -> 'a) -> 'a
(** [parse_import lexbuf env kontinue] parses an import statement. *)

val parse_pkg : Sedlexing.lexbuf -> Syntax.ty Env.t -> (Syntax.ty Env.t -> Syntax.pkg -> 'a) -> 'a
(** [parse_pkg lexbuf env kontinue] parses a package statement. *)
