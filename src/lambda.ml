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

open UntypedTree

let rec of_patterns = function
  | Pattern.Leaf label ->
      Leaf label
  | Pattern.Node (var, cases) ->
      let aux ((_, constr), tree) = (constr, of_patterns tree) in
      let cases = List.map aux cases in
      Node (var, cases)

let rec of_results m =
  let aux (acc, used_vars_acc) (wildcards, t) =
    let (t, used_vars) = of_typed_term t in
    let remove acc (_, name) = Set.remove name acc in
    let used_vars = List.fold_left remove used_vars wildcards in
    ((wildcards, t) :: acc, Set.union used_vars used_vars_acc)
  in
  let (a, b) = List.fold_left aux ([], Set.empty) m in
  (List.rev a, b)

and of_branches branches =
  let aux (acc, used_vars_acc) (name, t) =
    let (t, used_vars) = of_typed_term t in
    ((name, t) :: acc, Set.union used_vars used_vars_acc)
  in
  List.fold_left aux ([], Set.empty) branches

and of_typed_term = function
  | TypedTree.Abs (name, with_exn, t) ->
      let (t, used_vars) = of_typed_term t in
      let used_vars = Set.remove name used_vars in
      (Abs (name, with_exn, used_vars, t), used_vars)
  | TypedTree.TApp t
  | TypedTree.TAbs t ->
      of_typed_term t
  | TypedTree.App (f, with_exn, x) ->
      let (f, used_vars1) = of_typed_term f in
      let (x, used_vars2) = of_typed_term x in
      (App (f, with_exn, x), Set.union used_vars1 used_vars2)
  | TypedTree.Val name ->
      (Val name, Set.singleton name)
  | TypedTree.PatternMatching (t, results, patterns) ->
      let (t, used_vars1) = of_typed_term t in
      let (results, used_vars2) = of_results results in
      let patterns = of_patterns patterns in
      (PatternMatching (t, results, patterns), Set.union used_vars1 used_vars2)
  | TypedTree.Try (t, with_exn, branches) ->
      let (t, used_vars1) = of_typed_term t in
      let (branches, used_vars2) = of_branches branches in
      (Try (t, with_exn, branches), Set.union used_vars1 used_vars2)
  | TypedTree.Let (name, t, xs) ->
      let (t, used_vars1) = of_typed_term t in
      let (xs, used_vars2) = of_typed_term xs in
      let used_vars = Set.union used_vars1 (Set.remove name used_vars2) in
      (Let (name, t, xs), used_vars)
  | TypedTree.LetRec (name, t, xs) ->
      let (t, used_vars1) = of_typed_term t in
      let (xs, used_vars2) = of_typed_term xs in
      let used_vars =
        Set.union (Set.remove name used_vars1) (Set.remove name used_vars2)
      in
      (LetRec (name, t, xs), used_vars)
  | TypedTree.Fail name ->
      (Fail name, Set.empty)

let of_typed_variant acc i = function
  | TypedTree.Variant (name, ty_size) ->
      let variant =
        let rec aux params = function
          | 0 ->
              (Variant i, params)
          | n ->
              let name = Ident.Name.of_list [string_of_int n] in
              let params = Set.add name params in
              let (t, used_vars) = aux params (pred n) in
              let used_vars = Set.remove name used_vars in
              (Abs (name, false, used_vars, t), used_vars)
        in
        let (t, _) = aux Set.empty ty_size in
        Value (name, t)
      in
      variant :: acc

let rec of_typed_tree = function
  | TypedTree.Value (name, t) :: xs ->
      let (t, _) = of_typed_term t in
      Value (name, t) :: of_typed_tree xs
  | TypedTree.RecValue (name, t) :: xs ->
      let (t, _) = of_typed_term t in
      RecValue (name, t) :: of_typed_tree xs
  | TypedTree.Binding (name, value) :: xs ->
      Binding (name, value) :: of_typed_tree xs
  | TypedTree.Datatype variants :: xs ->
      let variants = List.fold_lefti of_typed_variant [] variants in
      let variants = List.rev variants in
      variants @ of_typed_tree xs
  | TypedTree.Exception name :: xs ->
      Exception name :: of_typed_tree xs
  | [] ->
      []
