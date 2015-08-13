(* Mixture_Applicative -- Applicative mixin

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Applicative mixin.

    Applicative is a functional programming structure popularised by
    Conor McBride and Ross Paterson.  This structure is a bit weaker
    than monads while being very close to them.

    {b See Also}
    {{:https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Applicative.html} Applicative structure in Haskell}. *)


(** Input signature of the functor [Mixture_Applicative.Make]. *)
module type Basis =
sig
  type (+'a) t
  (** The type of applicative structures. *)

  val return : 'a -> 'a t
  (** Lift a value. *)

  val apply : ('a -> 'b) t -> 'a t -> 'b t
  (** [apply f] sequence computations and combine their results with [f]. *)
end

(** Output signature of the functor [Mixture_Applicative.Make]. *)
module type Methods =
sig
  type (+'a) t
  (** The type of applicative structures. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f] is the function on computations deduced from [f]. *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** A version of [map] for binary functions. *)

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** A version of [map] for ternary functions. *)

  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  (** A version of [map] for quaternary functions. *)

  val dist : 'a t list -> 'a list t
  (** The applicative distributor for list, that is, the natural
      transformation of a list of computations in the computation of a
      list. *)

  (** Infix operators. *)
  module Infix :
  sig
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    (** A shorthand for [apply], the sequential application. *)

    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    (** A shorthand for [map]. *)

    val ( <* ) : 'a t -> 'b t -> 'a t
    (** Sequence actions, discarding the value of the first
        argument. *)

    val ( >* ) : 'a t -> 'b t -> 'b t
    (** Sequence actions, discarding the value of the second
        argument. *)
  end
end

(** Functor implementing applicative methods based on an applicative
    definition. *)
module Make(B:Basis): Methods
  with type 'a t := 'a B.t

(** Signature of applicative structures. *)
module type S =
sig
  type (+'a) t
  include Basis with type 'a t := 'a t
  include Methods with type 'a t := 'a t
end
