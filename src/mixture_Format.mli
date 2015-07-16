(* Mixture_Format -- Formatted mixin

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Formatted mixin. *)

(** Input signature of the functor [Mixture_Format.Make]. *)
module type Basis =
sig

  type t
  (** The type of formatted elements. *)

  val format : Format.formatter -> t -> unit
  (** [format fft a] pretty prints [a] on [fft]. *)

end

(** Output signature of the functor [Mixture_Format.Make]. *)
module type Methods =
sig
  type t

  val to_string : t -> string
  (** Convert to string. *)

  val output : out_channel -> t -> unit
  (** Output on the given output channel. *)

  val print : t -> unit
  (** Output on the standard output channel. *)

end

(** Functor implementing output mixins based on a format definition. *)
module Make(B:Basis): Methods
  with type t := B.t

(** Signature of formatted mixins. *)
module type S =
sig
  type t
  include Basis with type t := t
  include Methods with type t := t
end
