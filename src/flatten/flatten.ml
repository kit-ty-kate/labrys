(*
Copyright (c) 2013-2016 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

open Containers
open Monomorphic.None

open FlattenTree

let rec of_term = function
  | LambdaTree.Abs (name, t) ->
      let t = of_term t in
      ([], Abs (name, t))
  | LambdaTree.Rec (name, t) ->
      let t = of_term t in
      ([], Rec (name, t))
  | LambdaTree.App (x, y) ->
      ([], App (x, y))
  | LambdaTree.Val name ->
      ([], Val name)
  | LambdaTree.Datatype (idx, args) ->
      ([], Datatype (idx, args))
  | LambdaTree.CallForeign (name, ret, args) ->
      ([], CallForeign (name, ret, args))
  | LambdaTree.PatternMatching (name, branches, default, tree) ->
      let branches = List.map of_term branches in
      let default = of_term default in
      ([], PatternMatching (name, branches, default, tree))
  | LambdaTree.Let (name, x, t) ->
      let (lets_x, x) = of_term x in
      let (lets_t, t) = of_term t in
      (lets_x @ [(name, x)] @ lets_t, t)
  | LambdaTree.Fail (name, args) ->
      ([], Fail (name, args))
  | LambdaTree.Try (t, (name, t')) ->
      let t = of_term t in
      let t' = of_term t' in
      ([], Try (t, (name, t')))
  | LambdaTree.RecordGet (name, idx) ->
      ([], RecordGet (name, idx))
  | LambdaTree.Const c ->
      ([], Const c)
  | LambdaTree.Unreachable ->
      ([], Unreachable)
  | LambdaTree.Reraise name ->
      ([], Reraise name)

let of_lambda_tree top =
  let aux = function
    | LambdaTree.Value (name, t, linkage) ->
        let t = of_term t in
        Value (name, t, linkage)
    | LambdaTree.Exception name ->
        Exception name
  in
  List.map aux top
