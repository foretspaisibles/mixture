(* Mixture_Compare -- Comparable mixin

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module type Basis =
sig
  type t
  val compare : t -> t -> int
end

module type Methods =
sig
  type t
  val equal: t -> t -> bool
  val eq: t -> t -> bool
  val neq: t -> t -> bool
  val gt: t -> t -> bool
  val ge: t -> t -> bool
  val lt: t -> t -> bool
  val le: t -> t -> bool
  val max : t -> t -> t
  val min : t -> t -> t

  module Infix :
  sig
    val (=): t -> t -> bool
    val (<>): t -> t -> bool
    val (>): t -> t -> bool
    val (>=): t -> t -> bool
    val (<): t -> t -> bool
    val (<=): t -> t -> bool
  end

end

module Make(B:Basis) =
struct
  let equal a b = (B.compare a b) = 0
  let eq a b = (B.compare a b) = 0
  let neq a b = not(eq a b)
  let gt a b = (B.compare a b) > 0
  let lt a b = (B.compare a b) < 0
  let ge a b = not(lt a b)
  let le a b = not(gt a b)
  let max a b = if ge a b then a else b
  let min a b = if le a b then a else b

  module Infix =
  struct
    let (=) = eq
    let (<>) = neq
    let (>) = gt
    let (>=) = ge
    let (<) = lt
    let (<=) = le
  end

end

module type S =
sig
  type t
  include Basis with type t := t
  include Methods with type t := t
end
