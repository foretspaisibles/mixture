(* Mixture_FoldAssoc -- Foldable associative mixin

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Foldable associative mixin.

    A foldable associative mixin is an associative container structure
    that can be walked through, accumulating observation of bindings in a
    state variable.  This operation is known as folding or reducing. *)

(** Input signature of the functor [Mixture_FoldAssoc.Make]. *)
module type Basis =
sig

  type (+'a) t
  (** The type of foldable mixins containing values of type ['a]. *)

  type key
  (** The type of keys indexing elements of the mixin. *)

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f cont acc] apply [f] on each binding of [cont],
      accumulating results in [acc]. *)

end

(** Output signature of the functor [Mixture_FoldAssoc.Make]. *)
module type Methods =
sig

  type (+'a) t
  (** The type of assoc foldable mixins containing
      values of type ['a]. *)

  type key
  (** The type of keys indexing elements of the mixin. *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  (** Folding without accumulation. *)

  val elements : 'a t -> 'a list
  (** Return the elements of the container. *)

  val bindings : 'a t -> (key * 'a) list
  (** Return the bindings of the container. *)

  val cardinal : 'a t -> int
  (** Return the number of elements in the container. *)

  val exists : (key -> 'a -> bool) -> 'a t -> bool
  (** [exists p cont] is true iff [cont] contains a binding satisfying [p]. *)

  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  (** [for_all p cont] is true iff each binding of [cont] satisfies [p]. *)

end

(** Functor implementing foldable mixins based on a folding function. *)
module Make(B:Basis): Methods
  with type 'a t := 'a B.t
   and type key := B.key

(** Signature of foldable assoc mixins. *)
module type S =
sig
  type (+'a) t
  type key
  include Basis
    with type 'a t := 'a t
     and type key := key
  include Methods
    with type 'a t := 'a t
     and type key := key
end
