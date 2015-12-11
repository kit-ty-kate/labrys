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

let replace ~from ~by =
  let eq = Ident.Name.equal from in
  let rec aux = function
    | Let (name, is_rec, t, xs) when eq name -> Let (name, is_rec, aux t, xs)
    | Let (name, is_rec, t, xs) -> Let (name, is_rec, aux t, aux xs)
    | App (f, x) -> App (aux f, aux x)
    | Abs (name, _, _) as t when eq name -> t
    | Abs (name, used_vars, t) -> Abs (name, used_vars, aux t)
    | Val name when eq name -> by
    | Val _ | Const _ | CallForeign _ as t -> t
    | Datatype (idx, fields) -> Datatype (idx, List.map aux fields)
    | PatternMatching (t, results, patterns) ->
      let aux' (vars, t) = (vars, aux t) in
        PatternMatching (aux t, List.map aux' results, patterns)
    | Try (t, patterns) ->
        let aux' (vars, t) = (vars, aux t) in
        Try (aux t, List.map aux' patterns)
    | Fail (name, args) -> Fail (name, List.map aux args)
    | RecordGet (t, n) -> RecordGet (aux t, n)
  in
  aux

let rec reduce = function
  | Let (name, NonRec, t, xs) ->
      begin match reduce t with
      | Val _ | Datatype _ | Const _ as by -> reduce (replace ~from:name ~by xs)
      | Let _ | App _ | Abs _
      | PatternMatching _ | Try _ | Fail _
      | CallForeign _ | RecordGet _ as t -> Let (name, NonRec, t, reduce xs)
      end
  | App (f, x) ->
      begin match reduce f with
      | Abs (name, _, t) -> reduce (Let (name, NonRec, x, t))
      | Let _ | App _ | Val _ | Datatype _
      | Const _ | PatternMatching _ | Try _
      | Fail _ | CallForeign _ | RecordGet _ as f -> App (f, reduce x)
      end
  | Abs (name, used_vars, t) -> Abs (name, used_vars, reduce t)
  | Val _ | Datatype _ | Const _ as t -> t
  | CallForeign (name, ty, args) -> CallForeign (name, ty, args)
  | Let (name, Rec, t, xs) -> Let (name, Rec, reduce t, reduce xs)
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
