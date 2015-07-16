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
  val join : ('a t) t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind2 : 'a t -> 'b t -> ('a -> 'b -> 'c t) -> 'c t
  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t
  end
end

module Make(B:Basis) =
struct

  open B

  let join mm =
    bind mm (fun x -> x)

  let map f m =
    bind m (fun x -> return (f x))

  let bind2 m_a m_b f =
    bind m_a (fun a -> bind m_b (f a))

  module Infix =
  struct
    let ( >>= ) = bind
    let ( >> ) m f = bind m (fun _ -> f ())
  end

end


module type S =
sig
  type (+'a) t
  include Basis with type 'a t := 'a t
  include Methods with type 'a t := 'a t
end
