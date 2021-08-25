(**
 * {1 Abstract Syntax}
 *)

open Common

(**
 * {2 Syntax}
 *)

(** {3 Expressions} *)

type expr = private
  | Bool of {
      loc:   Loc.t; (** Location *)
      value: bool   (** Value *)
    } (** Boolean literal *)
  | Int of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** Integer literal *)
  | Long of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** Long literal *)
  | Float of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** Float literal *)
  | Double of {
      loc:    Loc.t; (** Location *)
      lexeme: string (** Lexeme *)
    } (** Double literal *)
  | Rune of {
      loc: Loc.t;    (** Location *)
      value: Uchar.t (** Value *)
    } (** Rune literal *)
  | String of {
      loc: Loc.t;         (** Location *)
      value: Uchar.t list (** Value *)
    } (** String literal *)
(** Expressions *)

type name = private
  | Name of {
      loc: Loc.t; (** Location *)
      id:  Sym.t  (** Identifier *)
    } (** A name *)
(** Import Names *)

(** {3 Imports} *)

type src = private
  | Source of {
      loc:  Loc.t; (** Location *)
      name: name   (** Name of the source *)
    } (** An import source *)
(** Import Sources *)

type from = private
  | From of {
      loc: Loc.t; (** Location *)
      src: src   (** Source to import from *)
    } (** From clause *)
(** From Clauses *)

type alias = private
  | Alias of {
      loc:   Loc.t;      (** Location *)
      pkg:   name;       (** Package to import *)
      alias: name option (** Optional local name *)
    } (** A package alias *)
(** Package Aliases *)

type pkgs = private
  | Packages of {
      loc:  Loc.t;     (** Location *)
      pkgs: alias list (** The packages to import *)
    } (** A package list *)
(** Package List *)

type import = private
  | Import of {
      loc:  Loc.t;       (** Location *)
      from: from option; (** Optional import source *)
      pkgs: pkgs         (** The packages to import *)
    } (** An import statement *)
(** Import Statements *)

(** {3 Package Statement} *)

type pkg = private
  | Package of {
      loc: Loc.t; (** Location *)
      id:  name   (** Name of the package *)
    } (** A package statement *)
(** Package Statements *)

(** {3 Source Files} *)

type file = private
  | File of {
      pkg:     pkg;        (** Package statement *)
      imports: import list (** Import statements *)
    } (** A source file *)
(** Source Files *)

(**
 * {2 Constructors}
 *)

(** {3 Expressions} *)

val bool : Loc.t -> bool -> expr
(** [bool loc value] constructs a boolean literal at location [loc] with value
    [value]. *)

val int : Loc.t -> string -> expr
(** [int loc lexeme] constructs an integer literal at location [loc] with lexeme
    [lexeme]. *)

val long : Loc.t -> string -> expr
(** [long loc lexeme] constructs a long literal at location [loc] with lexeme
    [lexeme]. *)

val float : Loc.t -> string -> expr
(** [float loc lexeme] constructs a float literal at location [loc] with lexeme
    [lexeme]. *)

val double : Loc.t -> string -> expr
(** [double loc lexeme] constructs a double literal at location [loc] with
    lexeme [lexeme]. *)

val rune : Loc.t -> Uchar.t -> expr
(** [rune loc value] constructs a rune literal at location [loc] with value
    [value]. *)

val string : Loc.t -> Uchar.t list -> expr
(** [string loc value] constructs a string literal at location [loc] with value
    [value]. *)

(** {3 Imports} *)

val name : Loc.t -> Sym.t -> name
(** [name loc id] constructs a name at location [loc] of the identifier [id]. *)

val src : Loc.t -> name -> src
(** [src loc name] constructs a source reference at location [loc] with the name
    [name]. *)

val from : Loc.t -> src -> from
(** [from_clause loc src] constructs a [from] clause at location [loc] importing
    from the source [src]. *)

val alias : Loc.t -> name -> name option -> alias
(** [alias loc pkg alias] constructs a package alias at location [loc] importing
    the package [pkg] with the optional local alias [alias]. *)

val pkgs : Loc.t -> alias list -> pkgs
(** [pkgs loc pkgs] constructs a list of package aliases as location [loc]. *)

val import : Loc.t -> from option -> pkgs -> import
(** [import loc from pkgs] constructs an import statement at location [loc]
    importing the packages [pkgs] from the optional source [from]. *)

(** {3 Package Statement} *)

val pkg : Loc.t -> name -> pkg
(** [pkg loc id] constructs a package statement at location [loc] with the name
    [id]. *)

(** {3 Source Files} *)

val file : pkg -> import list -> file
(** [file pkg imports] constructs a source file where [pkg] is the package
    statement and [imports] is the list of import statements. *)
