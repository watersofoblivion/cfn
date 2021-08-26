(**
 {1 Pretty-Printing}

 Pretty printing of source files.
 *)

open Format

(** {2 Types} *)

val ty : formatter -> Type.t -> unit
(** [ty fmt ty] pretty-prints the type [ty] to [fmt]. *)

(** {2 Expressions} *)

val expr : formatter -> Ast.expr -> unit
(** [expr fmt expr] pretty-prints the expression [expr] to [fmt]. *)

(** {2 Patterns} *)

val patt : formatter -> Ast.patt -> unit
(** [patt fmt patt] pretty-prints the pattern [patt] to [fmt]. *)

(** {2 Bindings} *)

val binding : formatter -> Ast.binding -> unit
(** [binding fmt binding] pretty-prints the binding [binding] to [fmt]. *)

(** {2 Top-Level Expressions} *)

val top : formatter -> Ast.top -> unit
(** [top fmt top] pretty-prints the top-level expression [top] to [fmt]. *)

(** {2 Imports} *)

val name : formatter -> Ast.name -> unit
(** [name fmt name] pretty-prints the import name [name] to [fmt]. *)

val src : formatter -> Ast.src -> unit
(** [src fmt src] pretty-prints the import source [src] to [fmt]. *)

val from : formatter -> Ast.from -> unit
(** [from fmt from] pretty-prints the from clause [from] to [fmt]. *)

val alias : formatter -> Ast.alias -> unit
(** [alias fmt alias] pretty-prints the import alias [alias] to [fmt]. *)

val pkgs : formatter -> Ast.pkgs -> unit
(** [pkgs fmt pkgs] pretty-prints the package list [pkgs] to [fmt]. *)

val import : formatter -> Ast.import -> unit
(** [import fmt import] pretty-prints the import statement [import] to [fmt]. *)

(** {2 Package Statement} *)

val pkg : formatter -> Ast.pkg -> unit
(** [pkg fmt pkg] pretty-prints the pkg statement [pkg] to [fmt]. *)

(** {2 Files} *)

val file : formatter -> Ast.file -> unit
(** [file fmt f] pretty-prints the file [f] to [fmt]. *)
