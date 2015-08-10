(* TestMonad -- Test monad structures

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Broken

module Stdlib_List = List

(* We implement several calssical monads. *)


module Maybe =
struct
  module Basis =
  struct
    type 'a t = 'a option

    let return x =
      Some(x)

    let bind m f =
      match m with
      | Some(x) -> f x
      | None -> None
  end

  module Methods =
    Mixture_Monad.Make(Basis)

  include Basis
  include Methods

  let format f pp m =
    let open Format in
    match m with
    | Some(x) -> fprintf pp "Some(%a)" f x
    | None -> fprintf pp "None"
end

module MaybeImplementsMonad =
  (Maybe : Mixture_Monad.S)

module MaybeImplementsApplicative =
  (Maybe : Mixture_Applicative.S)


module List =
struct

  module Basis =
  struct
    type 'a t = 'a list

    let return x =
      [x]

    let bind m f =
      Stdlib_List.concat (Stdlib_List.map f m)
  end

  let cons hd tl =
    hd :: tl

  let format f pp m =
    let open Format in
    let sep = ref "" in
    fprintf pp "[";
    List.iter (fun x -> fprintf pp "%s%a" (!sep) f x; sep := "; ") m;
    fprintf pp "]"

  include Stdlib_List
  module Methods =
    Mixture_Monad.Make(Basis)

  include Basis
  include Methods
end

module ListImplementsMonad =
  (List : Mixture_Monad.S)

module ListImplementsApplicative =
  (List : Mixture_Applicative.S)

let assert_maybe_string id ?expected_failure f a b =
  assert_equal
    id
    ?expected_failure
    ~printer:(Maybe.format Format.pp_print_string)
    ~equal:( (=) )
    f a b

let assert_list_string id ?expected_failure f a b =
  assert_equal
    id
    ?expected_failure
    ~printer:(List.format Format.pp_print_string)
    ~equal:( (=) )
    f a b

let () =
  suite "maybe" "Test the maybe monad" [

    assert_maybe_string "map"
      (Maybe.map String.uppercase) (Some "a") (Some "A");

    assert_maybe_string "map_infix"
      (Maybe.Infix.( <$> ) String.uppercase) (Some "a") (Some "A");

    assert_maybe_string "apply"
      (Maybe.apply (Some(String.uppercase))) (Some "a") (Some "A");

    assert_maybe_string "apply_infix"
      (Maybe.Infix.( <*> ) (Some(String.uppercase))) (Some "a") (Some "A");

    assert_maybe_string "apply_left_1"
      (Maybe.Infix.( <* ) None) (Some "b") None;

    assert_maybe_string "apply_left_2"
      (Maybe.Infix.( <* ) (Some "a")) (Some "b") (Some "a");

    assert_maybe_string "apply_right_1"
      (Maybe.Infix.( >* ) None) (Some "b") None;

    assert_maybe_string "apply_right_2"
      (Maybe.Infix.( >* ) (Some "a")) (Some "b") (Some "b");
  ];
  suite "list" "Test the list monad" [
    assert_list_string "cartesian_product"
      (List.bind2 ["1";"2";"3"] ["4";"5"])
      (fun x y -> [Printf.sprintf "(%s, %s)" x y])
      ["(1, 4)"; "(1, 5)"; "(2, 4)"; "(2, 5)"; "(3, 4)"; "(3, 5)"]
  ];
  package "monad" "Test all monad mixin features" [
    "list";
    "maybe";
  ]
