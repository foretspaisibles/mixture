(* Mixture_Format -- Formatted mixin

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
  val format : Format.formatter -> t -> unit
end

module type Methods =
sig
  type t
  val to_string : t -> string
  val output : out_channel -> t -> unit
  val print : t -> unit
end

module Make(B:Basis) =
struct
  open Format
  open B

  let to_string a =
    begin
      format str_formatter a;
      flush_str_formatter()
    end

  let output c a =
    output_string c (to_string a)

  let print a =
    output stdout a

end

module type S =
sig
  type t
  include Basis with type t := t
  include Methods with type t := t
end
