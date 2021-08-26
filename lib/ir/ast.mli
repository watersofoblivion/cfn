(** {1 Abstract Syntax} *)

open Common

(** {2 Syntax} *)

(** {3 Atoms} *)

type atom = private
  | Bool of {
      value: bool (** Value *)
    } (** Booleans *)
  | Int of {
      value: int32 (** Value *)
    } (** Integers *)
  | Long of {
      value: int64 (** Value *)
    } (** Longs *)
  | Float of {
      value: float (** Value *)
    } (** Floats *)
  | Double of {
      value: float (** Value *)
    } (** Doubles *)
  | Rune of {
      value: Uchar.t (** Value *)
    } (** Runes *)
  | String of {
      value: Uchar.t list (** Value *)
    } (** Strings *)
  | Ident of {
      id: Sym.t (** Identifier *)
    } (** Identifier *)
(** Atomic Values *)

(** {3 Expressions} *)

type expr = private
  | Atom of {
      atom: atom (** Atomic Value *)
    } (** Atomic Expression *)
(** Expressions *)

(** {3 Blocks} *)

type block = private
  | Expr of {
      expr: expr (** Expression *)
    } (** Expression Block *)
(** Block Values *)

(** {3 Patterns} *)

type patt = private
  | PattGround (** Ground *)
  | PattVar of {
      id: Sym.t (** Identifier *)
    } (** Variable *)
(** Patterns *)

(** {3 Bindings} *)

type binding = private
  | Binding of {
      patt:  patt;   (** Pattern *)
      ty:    Type.t; (** Type annotation *)
      value: expr    (** Value expression *)
    } (** Binding *)
(** Bindings *)

(** {3 Top-Level Bindings} *)

type top = private
  | Let of {
      binding: binding (** Binding *)
    } (** Let Binding *)
(** Top-Level Bindings *)

(** {2 Constructors} *)

(** {3 Atoms} *)

val atom_bool : bool -> atom
(** [atom_bool value] constructs a boolean literal atom with value [value]. *)

val atom_int : int32 -> atom
(** [atom_int value] constructs an integer literal atom with value [value]. *)

val atom_long : int64 -> atom
(** [atom_long value] constructs a long literal atom with value [value]. *)

val atom_float : float -> atom
(** [atom_float value] constructs a float literal atom with value [value]. *)

val atom_double : float -> atom
(** [atom_double value] constructs a double literal atom with value [value]. *)

val atom_rune : Uchar.t -> atom
(** [atom_rune value] constructs a rune literal atom with value [value]. *)

val atom_string : Uchar.t list -> atom
(** [atom_string value] constructs a string literal atom with value [value]. *)

val atom_ident : Sym.t -> atom
(** [atom_ident id] constructs an identifier atom with identifier [id]. *)

(** {3 Expressions} *)

val expr_atom : atom -> expr
(** [expr_atom atom] constructs an atomic value expression of the atom [atom]. *)

(** {3 Blocks} *)

val block_expr : expr -> block
(** [block_expr expr] constructs an expression block of the expression [expr]. *)

(** {3 Patterns} *)

val patt_ground : patt
(** [patt_ground] constructs a ground pattern. *)

val patt_var : Sym.t -> patt
(** [patt_var id] constructs a variable pattern binding [id]. *)

(** {3 Bindings} *)

val binding : patt -> Type.t -> expr -> binding
(** [binding patt ty value] constructs a binding that binds the value [value] of
    type [ty] to the pattern [patt]. *)

(** {3 Top-Level Bindings} *)

val top_let : binding -> top
(** [top_let binding] constructs a top-level value binding of the binding
    [binding]. *)
