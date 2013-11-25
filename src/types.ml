(*
Copyright (c) 2013 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Batteries
open MonadOpen

let fmt = Printf.sprintf

type t =
  | Fun of (t * t)
  | Ty of string
  | Forall of (string * t)

type env = (string * t)

let rec to_string = function
  | Fun (Ty x, ret) -> x ^ " -> " ^ to_string ret
  | Fun (x, ret) -> "(" ^ to_string x ^ ") -> " ^ to_string ret
  | Ty x -> x
  | Forall (x, t) -> fmt "forall %s. %s" x (to_string t)

let from_parse_tree gamma =
  let rec aux = function
    | ParseTree.Fun (x, y) ->
        aux x >>= fun x ->
        aux y >>= fun y ->
        Exn.return (Fun (x, y))
    | ParseTree.Ty name ->
        List.find (fun x -> Unsafe.(fst x = name)) gamma >>= fun x ->
        Exn.return (snd x)
  in
  aux

let equal x y =
  let rec aux = function
    | Fun (param, res), Fun (param', res') ->
        aux (param, param') && aux (res, res')
    | Ty x, Ty x' -> String.equal x x'
    | Forall (_, t), Forall (_, t') -> aux (t, t')
    | Forall _, Ty _
    | Ty _, Fun _
    | Ty _, Forall _
    | Fun _, Forall _
    | Fun _, Ty _
    | Forall _, Fun _ -> false
  in
  aux (x, y)

let replace ~from ~ty =
  let rec aux = function
    | Fun (param, ret) -> Fun (aux param, aux ret)
    | Ty x when String.equal x from -> ty
    | Ty x -> Ty x
    | Forall (x, _) as ty when String.equal x from -> ty
    | Forall (x, t) -> Forall (x, aux t)
  in
  aux
