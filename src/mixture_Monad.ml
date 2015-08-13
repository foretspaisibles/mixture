(* Mixture_Monad -- Monadic mixin

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
  type (+'a) t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type Methods =
sig
  type (+'a) t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val join : ('a t) t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind2 : 'a t -> 'b t -> ('a -> 'b -> 'c t) -> 'c t
  val bind2 : 'a t -> 'b t -> ('a -> 'b -> 'c t) -> 'c t
  val bind3 : 'a t -> 'b t -> 'c t -> ('a -> 'b -> 'c -> 'd t) -> 'd t
  val bind4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a -> 'b -> 'c -> 'd -> 'e t) -> 'e t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t 
  val dist : 'a t list -> 'a list t
  val ignore : 'a t -> unit t
  val filter : ('a -> bool t) -> 'a t list -> 'a list t
  val only_if : bool -> unit t -> unit t
  val unless : bool -> unit t -> unit t
  module Infix : sig
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <* ) : 'a t -> 'b t -> 'a t
    val ( >* ) : 'a t -> 'b t -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t
    val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
    val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> ('a -> 'c t)
  end
end

module Make(B:Basis) =
struct

  module Internal =
  struct
    include B
    let apply fm xm =
      bind fm (fun f -> bind xm (fun x -> return (f x)))
  end

  module ApplicativeMethods =
    Mixture_Applicative.Make(Internal)

  module Infix =
  struct
    let ( >>= ) =
      B.bind

    let ( >> ) m f =
      B.bind m (fun _ -> f ())

    let ( >=> ) g f x =
      B.bind (g x) f

    let ( <=< ) f g x =
      B.bind (g x) f

    let ( <*> ) =
      ApplicativeMethods.Infix.( <*> )

    let ( <$> ) f m =
      B.(m >>= fun x -> return(f x))

    let ( <* ) =
      ApplicativeMethods.Infix.( <* )

    let ( >* ) =
      ApplicativeMethods.Infix.( >* )
  end

  let apply =
    Internal.apply

  let join mm =
    Infix.(mm >>= (fun x -> x))

  let map f x =
    Infix.(f <$> x)

  let map2 f x y =
    Infix.(f <$> x <*> y)

  let map3 f x y z =
    Infix.(f <$> x <*> y <*> z)

  let map4 f x y z t =
    Infix.(f <$> x <*> y <*> z <*> t)

  let bind2 m_a m_b f =
    Infix.(m_a >>= fun a -> m_b >>= (f a))

  let bind3 m_a m_b m_c f =
    Infix.(m_a >>= fun a -> m_b >>= fun b -> m_c >>= (f a b))

  let bind4 m_a m_b m_c m_d f =
    Infix.(m_a >>= fun a -> m_b >>= fun b -> m_c >>= fun c -> m_d >>= (f a b c))

  let _cons hd tl =
    hd :: tl

  let rec dist =
    function
    | [] -> B.return []
    | hd :: tl -> Infix.(_cons <$> hd <*> (dist tl))

  let ignore m =
    Infix.(Pervasives.ignore <$> m)

  let filter pred lst =
    let rec loop m = function
      | [] -> Infix.(List.rev <$> m)
      | hd :: tl ->
          Infix.(hd >>= pred >>= maybe_pack m hd tl)
    and maybe_pack m hd tl flag =
      if flag then
        loop Infix.(_cons <$> hd <*> m) tl
      else
        loop m tl
    in
    loop (B.return []) lst

  let only_if flag m =
    if flag then m else (B.return ())

  let unless flag m =
    if flag then (B.return ()) else m
end

module type S =
sig
  type (+'a) t
  include Basis with type 'a t := 'a t
  include Methods with type 'a t := 'a t
end
