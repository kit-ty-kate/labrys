(*
Copyright (c) 2013-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

open Monomorphic_containers.Open

open LambdaTree

let rec reduce_abs x = function
  | Abs (name, _, t) -> Let (name, NonRec, Val x, t)
  | Let (name, is_rec, y, t) -> Let (name, is_rec, y, reduce_abs x t)
  | App _ | Val _ | Datatype _
  | Const _ | PatternMatching _ | Try _
  | Fail _ | CallForeign _ | RecordGet _ as f -> App (f, x)

let rec reduce = function
  | App (f, x) -> reduce_abs x (reduce f)
  | Abs (name, used_vars, t) -> Abs (name, used_vars, reduce t)
  | Val _ | Datatype _ | Const _ as t -> t
  | CallForeign (name, ty, args) -> CallForeign (name, ty, args)
  | Let (name, is_rec, t, xs) -> Let (name, is_rec, reduce t, reduce xs)
  | PatternMatching (t, results, patterns) ->
      let aux (vars, t) = (vars, reduce t) in
      PatternMatching (reduce t, List.map aux results, patterns)
  | Try (t, patterns) ->
      let aux (vars, t) = (vars, reduce t) in
      Try (reduce t, List.map aux patterns)
  | Fail (name, args) -> Fail (name, List.map reduce args)
  | RecordGet (t, n) -> RecordGet (reduce t, n)

let reduce =
  let aux = function
    | Value (name, t, linkage) -> Value (name, reduce t, linkage)
    | Function (name, (arg, t), link) -> Function (name, (arg, reduce t), link)
    | Exception _ as top -> top
  in
  List.map aux
