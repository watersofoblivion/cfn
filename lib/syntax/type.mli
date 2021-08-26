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
