(* Mixture_FoldRight --  Right foldable mixin

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
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end


module type Methods =
sig
  type (+'a) t
  val iter : ('a -> unit) -> 'a t -> unit
  val elements : 'a t -> 'a list
  val cardinal : 'a t -> int
  val exists : ('a -> bool) -> 'a t -> bool
  val for_all : ('a -> bool) -> 'a t -> bool
end


module Make(B:Basis) =
struct

  let iter f cont =
    let loop a () = f a in
    B.fold loop cont ()

  let elements cont =
    let loop a acc = a :: acc in
    B.fold loop cont []

  let cardinal cont =
    B.fold (fun _ acc -> succ acc) cont 0

  let exists p cont =
    let loop a =
      if p a then raise Mixture_Exit else ()
    in
    try (iter loop cont; false)
    with Mixture_Exit -> true

  let for_all p cont =
    let loop a =
      if p a then () else raise Mixture_Exit
    in
    try (iter loop cont; true)
    with Mixture_Exit -> false

end

module type S =
sig
  type (+'a) t
  include Basis with type 'a t := 'a t
  include Methods with type 'a t := 'a t
end
