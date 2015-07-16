(* Mixture_Parse -- Parsable mixin

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
  val path : string
  val parse : Lexing.lexbuf -> t
end

module type Methods =
sig
  type t
  val of_string : string -> t
end


module Make(B:Basis) =
struct
  let of_string s =
    try B.parse (Lexing.from_string s)
    with Failure _ -> failwith (B.path ^ ".of_string")
end

module type S =
sig
  type t
  include Basis with type t := t
  include Methods with type t := t
end
