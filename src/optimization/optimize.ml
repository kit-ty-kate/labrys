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

open OptimizedTree

module Set = GammaSet.IDValue

let rec of_term' = function
  | FlattenTree.Abs (name, t) ->
      let (t, fv) = of_term t in
      let fv = Set.remove name fv in
      (Abs (name, fv, t), fv)
  | FlattenTree.App (x, y) ->
      (App (x, y), Set.of_list [x; y])
  | FlattenTree.Val name ->
      (Val name, Set.singleton name)
  | FlattenTree.Datatype (idx, args) ->
      (Datatype (idx, args), Set.of_list args)
  | FlattenTree.CallForeign (name, ty, args) ->
      let fv = List.fold_right (fun (_, name) -> Set.add name) args Set.empty in
      (CallForeign (name, ty, args), fv)
  | FlattenTree.PatternMatching (name, branches, default, tree) ->
      let (default, fv) = of_term default in
      let (branches, fv) = of_branches fv branches in
      (PatternMatching (name, branches, default, tree), Set.add name fv)
  | FlattenTree.Rec (name, t) ->
      let (t, fv) = of_term t in
      let fv = Set.remove name fv in
      (Rec (name, t), fv)
  | FlattenTree.Fail (exn, args) ->
      (Fail (exn, args), Set.of_list args)
  | FlattenTree.Try (t, (name, t')) ->
      let (t, fv1) = of_term t in
      let (t', fv2) = of_term t' in
      let fv2 = Set.remove name fv2 in
      (Try (t, (name, t')), Set.union fv1 fv2)
  | FlattenTree.RecordGet (name, idx) ->
      (RecordGet (name, idx), Set.singleton name)
  | FlattenTree.Const c ->
      (Const c, Set.empty)
  | FlattenTree.Unreachable ->
      (Unreachable, Set.empty)
  | FlattenTree.Reraise name ->
      (Reraise name, Set.singleton name)

and of_term (lets, t) =
  let rec aux = function
    | (name, x)::xs ->
        let (x, fv1) = of_term' x in
        let (lets, t, fv2) = aux xs in
        let fv = Set.union fv1 (Set.remove name fv2) in
        ((name, x) :: lets, t, fv)
    | [] ->
        let (t, fv) = of_term' t in
        ([], t, fv)
  in
  let (lets, t, fv) = aux lets in
  ((lets, t), fv)

and of_branches fv branches =
  let aux (acc, fv) t =
    let (t, fvt) = of_term t in
    (t :: acc, Set.union fvt fv)
  in
  let (branches, fv) = List.fold_left aux ([], fv) branches in
  (List.rev branches, fv)

let of_flatten_tree tree =
  let aux = function
    | FlattenTree.Value (name, t, linkage) ->
        let (t, _) = of_term t in
        Value (name, t, linkage)
    | FlattenTree.Exception exn ->
        Exception exn
  in
  List.map aux tree
