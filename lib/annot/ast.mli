(** {1 Abstract Syntax} *)

open Common

(** {2 Syntax} *)

(** {3 Expressions} *)

type expr = private
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
(** Expressions *)

(** {3 Patterns} *)

type patt = private
  | PattGround (** Ground *)
  | PattVar of {
      id: Sym.t  (** Identifier *)
    } (** Variable *)
(** Patterns *)

(** {3 Bindings} *)

type binding = private
  | Binding of {
      patt:  patt;   (** Pattern *)
      ty:    Type.t; (** Type annotation *)
      value: expr;   (** Value expression *)
    }
(** Bindings *)

(** {3 Top-Level Bindings} *)

type top = private
  | Let of {
      binding: binding (** Binding *)
    } (** Let Binding *)
(** Top-Level Bindings *)

(** {2 Constructors} *)

(** {3 Expressions} *)

val bool : bool -> expr
(** [bool value] constructs a boolean literal with value [value]. *)

val int : int32 -> expr
(** [int value] constructs an integer literal with value [value]. *)

val long : int64 -> expr
(** [long value] constructs a long literal with value [value]. *)

val float : float -> expr
(** [float value] constructs a float literal with value [value]. *)

val double : float -> expr
(** [double value] constructs a double literal with value [value]. *)

val rune : Uchar.t -> expr
(** [rune value] constructs a rune literal with value [value]. *)

val string : Uchar.t list -> expr
(** [string value] constructs a string literal with value [value]. *)

(** {3 Patterns} *)

val patt_ground : patt
(** [patt_ground] constructs a ground pattern. *)

val patt_var : Sym.t -> patt
(** [patt_var id] constructs a variable pattern binding the identifier [id]. *)

(** {3 Bindings} *)

val binding : patt -> Type.t -> expr -> binding
(** [binding patt ty expr] constructs a binding that binds the value [expr] of
    type [ty] to the pattern [patt]. *)

(** {3 Top-Level Bindings} *)

val top_let : binding -> top
(** [top_let patt ty value] constructs a top-level let binding of the binding
    [binding]. *)
