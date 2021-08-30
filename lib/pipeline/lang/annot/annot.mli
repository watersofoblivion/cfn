(** {1 Annotated Syntax} *)

open Format

open Common

(** {2 Syntax} *)

type ty = private
  | TyBool   (** Boolean *)
  | TyInt    (** Integer *)
  | TyLong   (** Long *)
  | TyFloat  (** Float *)
  | TyDouble (** Double *)
  | TyRune   (** Rune *)
  | TyString (** String *)
(** Types *)

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
      value: string; (** UTF-8 encoded value *)
      len:   int     (** Length in runes *)
    } (** Strings *)
  | Ident of {
      id: Sym.t (** Identifier *)
    } (** Identifiers *)
(** Expressions *)

type patt = private
  | PattGround (** Ground *)
  | PattVar of {
      id: Sym.t  (** Identifier *)
    } (** Variable *)
(** Patterns *)

type binding = private
  | Binding of {
      patt:  patt; (** Pattern *)
      ty:    ty;   (** Type annotation *)
      value: expr; (** Value expression *)
    }
(** Bindings *)

type top = private
  | Let of {
      binding: binding (** Binding *)
    } (** Let Binding *)
(** Top-Level Bindings *)

(** {2 Constructors} *)

(** {3 Types} *)

val ty_bool : ty
(** [ty_bool] constructs a boolean type. *)

val ty_int : ty
(** [ty_int] constructs an integer type. *)

val ty_long : ty
(** [ty_long] constructs a long type. *)

val ty_float : ty
(** [ty_float] constructs a float type. *)

val ty_double : ty
(** [ty_double] constructs a double type. *)

val ty_rune : ty
(** [ty_rune] constructs a rune type. *)

val ty_string : ty
(** [ty_string] constructs a string type. *)

(** {3 Expressions} *)

val expr_bool : bool -> expr
(** [expr_bool value] constructs a boolean literal with value [value]. *)

val expr_int : int32 -> expr
(** [expr_int value] constructs an integer literal with value [value]. *)

val expr_long : int64 -> expr
(** [expr_long value] constructs a long literal with value [value]. *)

val expr_float : float -> expr
(** [expr_float value] constructs a float literal with value [value]. *)

val expr_double : float -> expr
(** [expr_double value] constructs a double literal with value [value]. *)

val expr_rune : Uchar.t -> expr
(** [expr_rune value] constructs a rune literal with value [value]. *)

val expr_string : string -> expr
(** [expr_string value] constructs a string literal with value [value].  The
    value is normalized and the length is computed. *)

val expr_ident : Sym.t -> expr
(** [expr_ident id] constructs an identifier literal with the identifier [id]. *)

(** {3 Patterns} *)

val patt_ground : patt
(** [patt_ground] constructs a ground pattern. *)

val patt_var : Sym.t -> patt
(** [patt_var id] constructs a variable pattern binding the identifier [id]. *)

(** {3 Bindings} *)

val binding : patt -> ty -> expr -> binding
(** [binding patt ty expr] constructs a binding that binds the value [expr] of
    type [ty] to the pattern [patt]. *)

(** {3 Top-Level Bindings} *)

val top_let : binding -> top
(** [top_let patt ty value] constructs a top-level let binding of the binding
    [binding]. *)

(** {2 Operations} *)

(** {3 Type Equality} *)

val ty_equal : ty -> ty -> bool
(** [ty_equal ty ty'] tests if type [ty] is equal to type [ty']. *)

(** {3 Pretty Printing} *)

val pp_ty : formatter -> ty -> unit
(** [pp_ty fmt ty] pretty-prints the type [ty] to the formatter [fmt]. *)

val pp_expr : formatter -> expr -> unit
(** [pp_expr fmt expr] pretty-prints the expression [expr] to the formatter
    [fmt]. *)

val pp_patt : formatter -> patt -> unit
(** [pp_patt fmt patt] pretty-prints the pattern [patt] to the formatter [fmt]. *)

val pp_binding : formatter -> binding -> unit
(** [pp_binding fmt binding] pretty-prints the binding [binding] to the
    formatter [fmt]. *)

val pp_top : formatter -> top -> unit
(** [pp_top fmt top] pretty-prints the top-level expression [top] to the
    formatter [fmt]. *)

(** {3 Type Checking} *)

exception UnboundIdentifier of Sym.t
(** [UnboundIdentifier id] is raised when the identifier [id] is unbound. *)

exception MismatchedTypes of ty * ty
(** [MismatchedTypes (inferred, annotated)] is raised when the [inferred] type
    and the [annotated] type disagree. *)

val check_expr : ty Env.t -> expr -> (ty -> 'a) -> 'a
(** [check_expr env expr kontinue] type-checks the expression [expr] in the
    environment [env].  The type of the expression is passed to the continuation
    [kontinue]. *)

val check_patt : ty Env.t -> patt -> ty -> (ty Env.t -> 'a) -> 'a
(** [check_patt env patt ty kontinue] type-checks the pattern [patt] against the
    type [ty] in the environment [env].  A (possibly updated) environment is
    passed to the continuation [kontinue]. *)

val check_binding : ty Env.t -> binding -> (ty Env.t -> 'a) -> 'a
(** [check_binding env binding kontinue] type-checks the binding [binding] in
    the environment [env].  A (possibly updated) environment is passed to the
    continuation [kontinue]. *)

val check_top : ty Env.t -> top -> (ty Env.t -> 'a) -> 'a
(** [check_top env top kontinue] type-checks the top-level expression [top] in
    the environment [env].  A (possibly updated) environment is passed to the
    continuation [kontinue]. *)
