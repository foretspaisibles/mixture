(* Mixture_Applicative -- Applicatives

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
  val return : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type Methods =
sig
  type (+'a) t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val dist : 'a t list -> 'a list t
  module Infix :
  sig
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <* ) : 'a t -> 'b t -> 'a t
    val ( >* ) : 'a t -> 'b t -> 'b t
  end
end



module Make(B:Basis) =
struct

  open B

  module Infix =
  struct
    let ( <*> ) =
      apply

    let ( <$> ) f x =
      apply (return f) x

    let ( <* ) m_a m_b =
      (fun x _ -> x) <$> m_a <*> m_b

    let ( >* ) m_a m_b =
        (fun _ y -> y) <$> m_a <*> m_b
  end

  let map f x =
    Infix.(f <$> x)

  let map2 f x y =
    Infix.(f <$> x <*> y)

  let map3 f x y z =
    Infix.(f <$> x <*> y <*> z)

  let map4 f x y z t =
    Infix.(f <$> x <*> y <*> z <*> t)

  let rec dist =
    let cons hd tl =
      hd :: tl
    in
    function
    | [] -> return []
    | hd :: tl -> Infix.(cons <$> hd <*> (dist tl))
end

module type S =
sig
  type (+'a) t
  include Basis with type 'a t := 'a t
  include Methods with type 'a t := 'a t
end
