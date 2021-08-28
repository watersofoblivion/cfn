(** {1 Types} *)

open Common

type t = private
  | Constr of {
      loc: Loc.t; (** Location *)
      id:  Sym.t  (** Constructor *)
    } (** Type Constructor *)
(** Types *)

(** {2 Constructors} *)

val constr : Loc.t -> Sym.t -> t
(** [constr loc id] constructs a type constructor at location [loc] for the type
    [id]. *)

(** {2 Operations} *)

(** {3 Equality} *)

val equal : t -> t -> bool
(** [equal ty ty'] tests if the type [ty] is equal to the type [ty']. *)

(** {3 Location} *)

val loc_ty : t -> Loc.t
(** [loc_ty ty] returns the location of the type [ty]. *)
