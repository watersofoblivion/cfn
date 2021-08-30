(** {1 Monomorphic} *)

open Format

open Common

(**
 * {2 Hashing}
 *
 * One-at-a-time hashing (taken from
 * {{:https://eternallyconfuzzled.com/hashing-c-introduction-to-hashing}here}.)
 *)

type hash_block
(** A hash block *)

val hash_block : hash_block
(** [hash_block] is the empty hash block. *)

val hash_bool : bool -> hash_block -> hash_block
(** [hash_bool b blk] hashes [b] into [blk]. *)

val hash_char : char -> hash_block -> hash_block
(** [hash_char c blk] hashes [c] into [blk]. *)

val hash_int : int -> hash_block -> hash_block
(** [hash_int i blk] hashes [i] into [blk]. *)

val hash_int32 : int32 -> hash_block -> hash_block
(** [hash_int32 i blk] hashes [i] into [blk]. *)

val hash_int64 : int64 -> hash_block -> hash_block
(** [hash_int64 i blk] hashes [i] into [blk]. *)

val hash_float : float -> hash_block -> hash_block
(** [hash_float f blk] hashes [f] into [blk]. *)

val hash_string : string -> hash_block -> hash_block
(** [hash_string s blk] hashes [s] into [blk]. *)

val hash_code : hash_block -> int
(** [hash_code blk] finalizes [blk] and returns the hash code. *)

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
      value: string; (** UTF-8 encoded value *)
      len:   int     (** Length in runes *)
    } (** Strings *)
  | Ident of {
      id: Sym.t (** Identifier *)
    } (** Identifier *)
(** Atomic Values *)

type expr = private
  | Atom of {
      atom: atom (** Atomic Value *)
    } (** Atomic Expression *)
(** Expressions *)

type block = private
  | Expr of {
      expr: expr (** Expression *)
    } (** Expression Block *)
(** Block Values *)

type patt = private
  | PattGround (** Ground *)
  | PattVar of {
      id: Sym.t (** Identifier *)
    } (** Variable *)
(** Patterns *)

type binding = private
  | Binding of {
      patt:  patt; (** Pattern *)
      ty:    ty;   (** Type annotation *)
      value: expr  (** Value expression *)
    } (** Binding *)
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

val atom_string : string -> atom
(** [atom_string value] constructs a string literal atom with value [value].
    The value is normalized and the length is computed. *)

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

val binding : patt -> ty -> expr -> binding
(** [binding patt ty value] constructs a binding that binds the value [value] of
    type [ty] to the pattern [patt]. *)

(** {3 Top-Level Bindings} *)

val top_let : binding -> top
(** [top_let binding] constructs a top-level value binding of the binding
    [binding]. *)

(** {2 Operations} *)

(** {3 Type Equality} *)

val ty_equal : ty -> ty -> bool
(** [ty_equal ty ty'] tests if type [ty] is equal to type [ty']. *)

(** {3 Pretty Printing} *)

val pp_ty : formatter -> ty -> unit
(** [pp_ty fmt ty] pretty-prints the type [ty] to the formatter [fmt]. *)

val pp_atom : formatter -> atom -> unit
(** [pp_atom fmt atom] pretty-prints the atomic value [atom] to the formatter
    [fmt]. *)

val pp_expr : formatter -> expr -> unit
(** [pp_expr fmt expr] pretty-prints the expression [expr] to the formatter
    [fmt]. *)

val pp_block : formatter -> block -> unit
(** [pp_block fmt block] pretty-prints the block [block] to the formatter [fmt]. *)

val pp_patt : formatter -> patt -> unit
(** [pp_patt fmt patt] pretty-prints the pattern [patt] to the formatter [fmt]. *)

val pp_binding : formatter -> binding -> unit
(** [pp_bining fmt binding] pretty-prints the binding [binding] to the formatter
    [fmt]. *)

val pp_top : formatter -> top -> unit
(** [pp_top fmt top] pretty-prints the top-level expression [top] to the
    formatter [fmt]. *)

(** {3 Type Checking} *)

exception UnboundIdentifier of Sym.t
(** [UnboundIdentifier id] is raised when the identifier [id] is unbound. *)

exception MismatchedTypes of ty * ty
(** [MismatchedTypes (inferred, annotated)] is raised when the inferred type and
    the disagree. *)

val check_atom : ty Env.t -> atom -> (ty -> 'a) -> 'a
(** [check_atom env atom kontinue] type-checks the atomic value [atom] in the
    environment [env].  The type of the atom is passed to the continuation
    [kontinue]. *)

val check_expr : ty Env.t -> expr -> (ty -> 'a) -> 'a
(** [check_expr env expr kontinue] type-checks the expression [expr] in the
    environment [env].  The type of the expression is passed to the continuation
    [kontinue]. *)

val check_block : ty Env.t -> block -> (ty -> 'a) -> 'a
(** [check_block env block kontinue] type-checks the block [block] in the
    environment [env].  The type of the block is passed to the continuation
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
