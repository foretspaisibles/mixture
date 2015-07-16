(* Mixture_Compare -- Comparable mixin

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Comparable mixin. *)

(** Input signature of the functor [Mixture_Compare.Make]. *)
module type Basis =
sig

  type t
  (** The type of comparable elements. *)

  val compare : t -> t -> int
  (** [compare x y] returns [0] if [x] is equal to [y] , a negative
      integer if [x] is less than [y] , and a positive integer if [x] is
      greater than [y]. *)

end

(** Output signature of the functor [Mixture_Compare.Make]. *)
module type Methods =
sig

  type t
  (** The type of comparable elements. *)

  val equal: t -> t -> bool
  (** {i Equal}. *)

  (** {6 Comparison operators}

      All tautological relations between these comparison operators and the
      [compare] function are satisfied. *)

  val eq: t -> t -> bool
  (** {i Equal}. *)

  val neq: t -> t -> bool
  (** {i Equal}. *)

  val gt: t -> t -> bool
  (** {i Greater than}. *)

  val ge: t -> t -> bool
  (** {i Greater or equal to}. *)

  val lt: t -> t -> bool
  (** {i Lesser than}. *)

  val le: t -> t -> bool
  (** {i Lesser or equal to}. *)

  val max : t -> t -> t
  (** The maximum of two comparable elements. *)

  val min : t -> t -> t
  (** The minimum of two comparable elements. *)

  (** Operators for expression evaluations. *)
  module Infix :
  sig

    val (=): t -> t -> bool
    (** {i Equal}. *)

    val (<>): t -> t -> bool
    (** {i Equal}. *)

    val (>): t -> t -> bool
    (** {i Greater than}. *)

    val (>=): t -> t -> bool
    (** {i Greater or equal to}. *)

    val (<): t -> t -> bool
    (** {i Lesser than}. *)

    val (<=): t -> t -> bool
    (** {i Lesser or equal to}. *)

  end

end

(** Functor implementing comparison mixins based on a comparison
    definition. *)
module Make(B:Basis): Methods
  with type t := B.t

(** Signature of comparable mixins. *)
module type S =
sig
  type t
  include Basis with type t := t
  include Methods with type t := t
end
