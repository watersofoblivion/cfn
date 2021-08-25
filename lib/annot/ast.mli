(** {1 Abstract Syntax} *)

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
