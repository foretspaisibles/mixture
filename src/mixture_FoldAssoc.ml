(* Mixture_FoldAssoc -- Foldable associative mixin

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

exception Mixture_Exit
(* A private exception used by the for_all and exists functions. *)

module type Basis =
sig
  type (+'a) t
  type key
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end


module type Methods =
sig
  type (+'a) t
  type key
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val elements : 'a t -> 'a list
  val bindings : 'a t -> (key*'a) list
  val cardinal : 'a t -> int
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
end


module Make(B:Basis) =
struct

  let iter f cont =
    let loop k a () = f k a in
    B.fold loop cont ()

  let elements cont =
    let loop _ a acc = a :: acc in
    B.fold loop cont []

  let bindings cont =
    let loop k a acc = (k,a) :: acc in
    B.fold loop cont []

  let cardinal cont =
    B.fold (fun _ _ acc -> succ acc) cont 0

  let exists p cont =
    let loop k a =
      if p k a then raise Mixture_Exit else ()
    in
    try (iter loop cont; false)
    with Mixture_Exit -> true

  let for_all p cont =
    let loop k a =
      if p k a then () else raise Mixture_Exit
    in
    try (iter loop cont; true)
    with Mixture_Exit -> false

end

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
