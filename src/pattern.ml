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

open BatteriesExceptionless
open Monomorphic.None

module Matrix = struct
  type constr =
    | MConstr of (string * constr list)
    | MAny of string

  type 'a t = (constr * 'a) list

  let replace_ty _ _ = assert false

  let create' ~loc gammaC ty =
    let rec aux acc = function
      | ParseTree.Any name when List.is_empty acc ->
          MAny name
      | ParseTree.Any name ->
          Error.fail ~loc "'%s' can't be applied to something" name
      | ParseTree.TyConstr name ->
          (* TODO: Check in gammaC (or make a module with abstract type ? :) )
             And check type of the parameters
           *)
          MConstr (name, acc)
      | ParseTree.PatternApp (x, y) ->
          aux (acc @ [aux [] y]) x
      | ParseTree.PatternTApp (x, ty) ->
          replace_ty x ty
    in
    aux []

  let create ~loc gammaC ty term p = [(create' ~loc gammaC ty p, term)]

  let append ~loc gammaC ty term p patterns = patterns @ create ~loc gammaC ty term p

  let get_results m = List.map snd m
end

type constr =
  | Constr of string
  | Any of string

type var =
  | VLeaf
  | VNode of (int * var)

type t =
  | Node of (var * (constr * t) list)
  | Leaf of int

let create _ = assert false
