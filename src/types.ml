module Exn = MonadExn

open MonadStdlib
open Exn.Ops

type ty = (string * Backend.ty)

type t =
  | Fun of (t * t)
  | Ty of ty

let rec to_string = function
  | Fun (Ty (x, _), ret) -> x ^ " -> " ^ to_string ret
  | Fun (x, ret) -> "(" ^ to_string x ^ ") -> " ^ to_string ret
  | Ty (x, _) -> x

let from_parse_tree gamma =
  let rec aux = function
    | ParseTree.Fun (x, y) ->
        aux x >>= fun x ->
        aux y >>= fun y ->
        Exn.return (Fun (x, y))
    | ParseTree.Ty name ->
        List.find (fun x -> Unsafe.(fst x = name)) gamma >>= fun x ->
        Exn.return (Ty x)
  in
  aux

let gamma =
  [ ("Int", Backend.int)
  ]
