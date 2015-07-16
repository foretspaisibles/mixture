(* Mixture_Monad -- Monadic mixin

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Monadic mixin.

    This mixin describes monads in OCaml following the indications given by
    “A Monad Tutorial for OCaml.”

    A monad over type ['a] is an embedding of ['a] in a larger type ['a
    t].  It can be thought of as the type of outcomes for computations
    producing values of type ['a].  Operations on monads can often be
    interepreted in terms of categorical diagrams you can draw given the
    embedding ['a -> 'a t] or the identity of ['a t], for instance fibred
    products.

    {b References:}

    {{:http://blog.enfranchisedmind.com/2007/08/a-monad-tutorial-for-ocaml/}
    A Monad Tutorial for Ocaml}. *)


(** Input signature of the functor [Mixture_Monad.Make]. *)
module type Basis =
sig
  type (+'a) t
  (** The type of monads. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind m f] bind [f] to the monad [m]. *)

  val return : 'a -> 'a t
  (** [return a] embed the value [a] in the monad. *)

end

(** Output signature of the functor [Mixture_Monad.Make]. *)
module type Methods =
sig
  type (+'a) t
  (** The type of monads. *)

  val join : ('a t) t -> 'a t
  (** [join mm] bind [mm] to the identity, reducing the monad. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f] is the natural tranformation between monads,
      induced by [f]. *)

  val bind2 : 'a t -> 'b t -> ('a -> 'b -> 'c t) -> 'c t
  (** [bind2 m_a m_b f] is similar to bind, but works on two
      arguments.

      It has the effect of a fibered product. *)

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [ m >>= f] is equivalent to [bind m f]. *)

    val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t
    (** [m >> f] binds [m] to [f], a context function. *)

  end

end

(** Functor implementing monadic mixins based on a monadic definition. *)
module Make(B:Basis): Methods
  with type 'a t := 'a B.t

(** Signature of monadic mixins. *)
module type S =
sig
  type (+'a) t
  include Basis with type 'a t := 'a t
  include Methods with type 'a t := 'a t
end
