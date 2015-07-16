(* Mixture_NumericalFunction -- Numerical function mixin

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Numerical function mixin.

    A numerical function mixin represents a numerical function of a real
    value, defined on a compact interval. *)


(** Input signature of the functor [Mixture_NumericalFunction.Make]. *)
module type Basis =
sig

  type t
  (** The type of numerical functions. *)

  val range : t -> float * float
  (** The range [(range_min,range_max)] of the numerical function. *)

  val eval : t -> float -> float
  (** [eval f x] evaluate [f] at [x].
      @raise Out_of_range if [x] is not in the range of the function. *)

end


(** Output signature of the functor [Mixture_NumericalFunction.Make]. *)
module type Methods =
sig

  type t
  (** The type of numerical functions. *)


  (** {6 Range of the numerical function} *)

  val range_min : t -> float
  (** The smallest value of [x] for which the numerical
      function is defined. *)

  val range_max : t -> float
  (** The larges value of [x] for which the numerical
      function is defined. *)

  val is_within_range : t -> float -> bool
  (** Predicate recognising points lying in the range of
      a numerical function. *)


  (** {6 Walking through the values of the function} *)

  val fold_step : t -> float -> (float -> float -> 'a -> 'a) -> 'a -> 'a
  (** [fold_step f step loop acc] compute [loop x (f x) acc] for all [x] in
      the range of [f] with the given [step], accumulating state in [acc].

      If [step < 0.0] the range is explored backwards.

      @raise Invalid_argument if [step = 0.0]. *)

  val iter_step : t -> float -> (float -> float -> unit) -> unit
  (** [iter_step f step loop] compute [loop x (f x)] for all [x] in
      the range of [f] with the given [step].

      @raise Invalid_argument if [step <= 0.0]. *)

  val fold_ticks : t -> int -> (float -> float -> 'a -> 'a) -> 'a -> 'a
  (** [fold_ticks f ticks loop acc] compute [loop x (f x) acc] for [n]
      evenly spaced values of [x] in the range of [f],
      accumulating state in [acc].

      @raise Invalid_argument if [n] is smaller than [0]. *)

  val iter_ticks : t -> int -> (float -> float -> unit) -> unit
  (** [iter_ticks f ticks loop] compute [loop x (f x)] for [n]
      evenly spaced values of [x] in the range of [f].

      if [n < 0], the range is explored backwards.

      @raise Invalid_argument if [n = 0]. *)
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
