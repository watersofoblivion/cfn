(** {1 Types} *)

type t = private
  | Bool   (** Boolean *)
  | Int    (** Integer *)
  | Long   (** Long *)
  | Float  (** Float *)
  | Double (** Double *)
  | Rune   (** Rune *)
  | String (** String *)
(** Types *)

(** {2 Constructors} *)

val bool : t
(** [bool] constructs a boolean type. *)

val int : t
(** [int] constructs an integer type. *)

val long : t
(** [long] constructs a long type. *)

val float : t
(** [float] constructs a float type. *)

val double : t
(** [double] constructs a double type. *)

val rune : t
(** [rune] constructs a rune type. *)

val string : t
(** [string] constructs a string type. *)

(** {2 Operations} *)

val equal : t -> t -> bool
(** [equal ty ty'] tests if type [ty] is equal to type [ty']. *)
