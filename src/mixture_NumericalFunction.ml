(* Mixture_NumericalFunction -- Numerical function mixin

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
  val range : t -> float * float
  val eval : t -> float -> float
end

module type Methods =
sig
  type t
  val range_min : t -> float
  val range_max : t -> float
  val is_within_range : t -> float -> bool
  val fold_step : t -> float -> (float -> float -> 'a -> 'a) -> 'a -> 'a
  val iter_step : t -> float -> (float -> float -> unit) -> unit
  val fold_ticks : t -> int -> (float -> float -> 'a -> 'a) -> 'a -> 'a
  val iter_ticks : t -> int -> (float -> float -> unit) -> unit
end

module Make(B:Basis) =
struct
  open B

  let range_min f =
    fst(range f)

  let range_max f =
    snd(range f)

  let is_within_range f x =
    let (xmin, xmax) = range f in
    xmin <= x && x <= xmax

  let fold_step f step comp acc =
    let (xmin, xmax) = range f in
    let rec loop x test acc =
      if test x then
        loop (x +. step) test (comp x (eval f x) acc)
      else
        acc
    in
    if step > 0.0 then
      loop xmin (fun x -> x <= xmax) acc
    else if step < 0.0 then
      loop xmax (fun x -> x >= xmin) acc
    else
      invalid_arg "fold_step"

  let fold_ticks f n comp acc =
    let (xmin, xmax) = range f in
    let step =
      if n <> 0 then
        (xmax -. xmin) /. (float_of_int n)
      else
        invalid_arg "fold_ticks"
    in
    fold_step f step comp acc

  let iter_step f step comp =
    let comp' x y () = comp x y in
    if step <> 0.0 then
      fold_step f step comp' ()
    else
      invalid_arg "iter_step"

  let iter_ticks f n comp =
    let comp' x y () = comp x y in
    if n <> 0 then
      fold_ticks f n comp' ()
    else
      invalid_arg "iter_ticks"

end

module type S =
sig
  type t
  include Basis with type t := t
  include Methods with type t := t
end
