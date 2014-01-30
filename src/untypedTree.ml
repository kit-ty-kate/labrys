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
open Monomorphic.None

type t =
  | Abs of (string * t)
  | App of (t * t)
  | Val of string

type variant =
  | Variant of (string * int)

type top =
  | Value of (string * t * int)
  | Binding of (string * string)
  | Datatype of variant list

let rec of_typed_term = function
  | TypedTree.Abs ({TypedTree.param = {TypedTree.name; _}; _}, t) ->
      let (t, size) = of_typed_term t in
      (Abs (name, t), succ size)
  | TypedTree.TApp (_, t, _)
  | TypedTree.TAbs (_, t) ->
      of_typed_term t
  | TypedTree.App (_, f, x) ->
      let (f, size) = of_typed_term f in
      let (x, size') = of_typed_term x in
      (App (f, x), size + size')
  | TypedTree.Val {TypedTree.name; _} ->
      (Val name, 0)

let of_typed_variant = function
  | TypedTree.Variant (name, ty) -> Variant (name, Types.size ty)

let rec of_typed_tree = function
  | TypedTree.Value ({TypedTree.name; _}, t) :: xs ->
      let (t, size) = of_typed_term t in
      Value (name, t, size) :: of_typed_tree xs
  | TypedTree.Binding ({TypedTree.name; _}, value) :: xs ->
      Binding (name, value) :: of_typed_tree xs
  | TypedTree.Datatype variants :: xs ->
      Datatype (List.map of_typed_variant variants) :: of_typed_tree xs
  | [] ->
      []
