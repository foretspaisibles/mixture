(* TestApplicative -- Test applicative structures

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Broken

(* We implement Reader as an exmple of Applicative *)
module Reader =
struct
  type environment =
    (string * bool) list

  let fetch x env =
    List.assoc x env

  module Basis =
  struct
    type 'a t = environment -> 'a

    let return x =
      fun _ -> x

    let apply f x =
      fun env -> (f env) (x env)
  end

  module Methods =
    Mixture_Applicative.Make(Basis)

  include Basis
  include Methods
end

module ReaderImplementsApplicative =
  (Reader : Mixture_Applicative.S)

module BooleanExpression =
struct

  type expr =
    | Variable of string
    | Immediate of bool
    | Not of expr
    | And of expr * expr
    | Or of expr * expr

  let rec eval =
    let open Reader.Infix in
    function
    | Variable(name) -> Reader.fetch name
    | Immediate(b) -> (fun _ -> b)
    | Not(expr) -> not <$> (eval expr)
    | Or(a,b) -> ( || ) <$> (eval a) <*> (eval b)
    | And(a,b) -> ( && ) <$> (eval a) <*> (eval b)
end

let () =
  let expr =
    let open BooleanExpression in
    And(Or(Variable("a"), Not(Variable("b"))), Immediate(true))
  in
  make_suite "applicative" "Test all applicative mixin features"
  |@ [
    assert_true "1" (BooleanExpression.eval expr) [
      "a", true;
      "b", true;
    ]
  ]
  |> register
