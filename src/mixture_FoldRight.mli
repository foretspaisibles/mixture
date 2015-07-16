(* Mixture_FoldRight --  Right foldable mixin

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Right foldable mixin.

    A right foldable mixin is a container structure that can be walked through,
    accumulating observation of elements in a state variable.  This
    operation is known as folding or reducing. *)

(** Input signature of the functor [Mixture_FoldRight.Make]. *)
module type Basis =
sig

  type (+'a) t
  (** The type of foldable mixins containing values of type ['a]. *)

  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f cont acc] apply [f] on each element of [cont],
      accumulating results in [acc]. *)

end

(** Output signature of the functor [Mixture_FoldRight.Make]. *)
module type Methods =
sig

  type (+'a) t
  (** The type of right foldable mixins containing
      values of type ['a]. *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** Folding without accumulation. *)

  val elements : 'a t -> 'a list
  (** Return the elements of the container. *)

  val cardinal : 'a t -> int
  (** Return the number of elements in the container. *)

  val exists : ('a -> bool) -> 'a t -> bool
  (** [exists p cont] is true iff [cont] contains an element satisfying [p]. *)

  val for_all : ('a -> bool) -> 'a t -> bool
  (** [for_all p cont] is true iff each element of [cont] satisfies [p]. *)

end

(** Functor implementing foldable mixins based on a folding function. *)
module Make(B:Basis): Methods
  with type 'a t := 'a B.t

(** Signature of right foldable mixins. *)
module type S =
sig
  type (+'a) t
  include Basis with type 'a t := 'a t
  include Methods with type 'a t := 'a t
end
