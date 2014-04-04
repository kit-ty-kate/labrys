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

let get_index_from_name name gammaC =
  Option.default_delayed
    (fun () -> assert false)
    (Gamma.Constr.find name gammaC)

let of_constr gammaC = function
  | Pattern.Constr name -> Constr (get_index_from_name name gammaC)
  | Pattern.Any name -> Any name

let rec of_patterns gammaC = function
  | Pattern.Leaf label ->
      Leaf label
  | Pattern.Node (var, patterns) ->
      let patterns =
        let aux (constr, tree) =
          (of_constr gammaC constr, of_patterns gammaC tree)
        in
        List.map aux patterns
      in
      Node (var, patterns)

let rec of_map_patterns gammaC m =
  let aux i t (acc, size) =
    let (t, size') = of_typed_term gammaC t in
    (Pattern.Map.add i t acc, Int.max size size')
  in
  Pattern.Map.fold aux m (Pattern.Map.empty, 0)

and of_typed_term gammaC = function
  | TypedTree.Abs ({TypedTree.param = {TypedTree.name; _}; _}, t) ->
      let (t, size) = of_typed_term gammaC t in
      (Abs (name, t), succ size)
  | TypedTree.TApp (_, t, _)
  | TypedTree.TAbs (_, t) ->
      of_typed_term gammaC t
  | TypedTree.App (_, f, x) ->
      let (f, size) = of_typed_term gammaC f in
      let (x, size') = of_typed_term gammaC x in
      (App (f, x), size + size')
  | TypedTree.Val {TypedTree.name; _} ->
      (Val name, 0)
  | TypedTree.PatternMatching (t, map_patterns, patterns, _) ->
      let (t, size) = of_typed_term gammaC t in
      let (map_patterns, size') = of_map_patterns gammaC map_patterns in
      let patterns = of_patterns gammaC patterns in
      (PatternMatching (t, map_patterns, patterns), size + size')

let of_typed_variant (acc, i, gammaC) = function
  | TypedTree.Variant (name, ty) ->
      let variant =
        let rec aux = function
          | 0 -> Variant i
          | n -> Abs ("", aux (pred n))
        in
        let size = TypesBeta.size ty in
        let t = aux size in
        Value (name, t, size)
      in
      (variant :: acc, succ i, Gamma.Constr.add name i gammaC)

let of_typed_tree =
  let rec aux gammaC = function
    | TypedTree.Value ({TypedTree.name; _}, t) :: xs ->
        let (t, size) = of_typed_term gammaC t in
        Value (name, t, size) :: aux gammaC xs
    | TypedTree.Binding ({TypedTree.name; _}, value) :: xs ->
        Binding (name, value) :: aux gammaC xs
    | TypedTree.Datatype variants :: xs ->
        let (variants, _, gammaC) =
          List.fold_left of_typed_variant ([], 0, Gamma.Constr.empty) variants
        in
        variants @ aux gammaC xs
    | [] ->
        []
  in
  aux Gamma.Constr.empty
