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
    (Gamma.Index.find name gammaC)

let of_constr gammaC = function
  | Pattern.Constr name -> Constr (get_index_from_name name gammaC)
  | Pattern.Any name -> Any name

let rec of_patterns gammaC = function
  | Pattern.Leaf label ->
      Leaf label
  | Pattern.Node (var, default, cases) ->
      let aux (constr, tree) =
        (of_constr gammaC constr, of_patterns gammaC tree)
      in
      let default = aux default in
      let cases = List.map aux cases in
      Node (var, default, cases)

let rec of_results gammaC gammaD m =
  let aux (acc, used_vars_acc) (wildcards, t) =
    let (t, used_vars) = of_typed_term gammaC gammaD t in
    let used_vars = List.fold_left List.remove used_vars wildcards in
    (t :: acc, used_vars @ used_vars_acc)
  in
  List.fold_left aux ([], []) m

and of_typed_term gammaC gammaD = function
  | TypedTree.Abs ({TypedTree.param = {TypedTree.name; _}; _}, t) ->
      let (t, used_vars) = of_typed_term gammaC gammaD t in
      let used_vars = List.remove used_vars name in
      (Abs (name, used_vars, t), used_vars)
  | TypedTree.TApp (_, t, _)
  | TypedTree.TAbs (_, t) ->
      of_typed_term gammaC gammaD t
  | TypedTree.App (_, f, x) ->
      let (f, used_vars1) = of_typed_term gammaC gammaD f in
      let (x, used_vars2) = of_typed_term gammaC gammaD x in
      (App (f, x), used_vars1 @ used_vars2)
  | TypedTree.Val {TypedTree.name; _} ->
      (Val name, [name])
  | TypedTree.PatternMatching (t, patterns, _) ->
      let (patterns, results) = Pattern.Matrix.split patterns in
      let patterns = Pattern.create gammaD patterns in
      let (t, used_vars1) = of_typed_term gammaC gammaD t in
      let (results, used_vars2) = of_results gammaC gammaD results in
      let patterns = of_patterns gammaC patterns in
      (PatternMatching (t, results, patterns), used_vars1 @ used_vars2)
  | TypedTree.Let (name, t, xs, _) ->
      let (t, used_vars1) = of_typed_term gammaC gammaD t in
      let (xs, used_vars2) = of_typed_term gammaC gammaD xs in
      let used_vars = used_vars1 @ List.remove used_vars2 name in
      (Let (name, t, xs), used_vars)
  | TypedTree.LetRec (name, _, t, xs, _) ->
      let (t, used_vars1) = of_typed_term gammaC gammaD t in
      let (xs, used_vars2) = of_typed_term gammaC gammaD xs in
      let used_vars =
        List.remove used_vars1 name @ List.remove used_vars2 name
      in
      (LetRec (name, t, xs), used_vars)

let of_typed_variant ~datatype (acc, i, gammaC, gammaD) = function
  | TypedTree.Variant (name, ty) ->
      let variant =
        let rec aux params = function
          | 0 ->
              (Variant i, params)
          | n ->
              let name = Gamma.Name.of_list [string_of_int n] in
              let params = name :: params in
              let (t, used_vars) = aux params (pred n) in
              let used_vars = List.remove used_vars name in
              (Abs (name, used_vars, t), used_vars)
        in
        let size = TypesBeta.size ty in
        let (t, _) = aux [] size in
        Value (name, t)
      in
      let gammaC = Gamma.Index.add name i gammaC in
      let gammaD = Gamma.Constr.append datatype name gammaD in
      (variant :: acc, succ i, gammaC, gammaD)

let of_typed_tree =
  let rec aux gammaC gammaD = function
    | TypedTree.Value ({TypedTree.name; _}, t) :: xs ->
        let (t, _) = of_typed_term gammaC gammaD t in
        Value (name, t) :: aux gammaC gammaD xs
    | TypedTree.RecValue ({TypedTree.name; _}, t) :: xs ->
        let (t, _) = of_typed_term gammaC gammaD t in
        RecValue (name, t) :: aux gammaC gammaD xs
    | TypedTree.Binding ({TypedTree.name; _}, value) :: xs ->
        Binding (name, value) :: aux gammaC gammaD xs
    | TypedTree.Datatype (name, variants) :: xs ->
        let (variants, _, gammaC, gammaD) =
          let gammaC = Gamma.Index.empty in
          let gammaD = Gamma.Constr.empty in
          List.fold_left (of_typed_variant ~datatype:name) ([], 0, gammaC, gammaD) variants
        in
        variants @ aux gammaC gammaD xs
    | [] ->
        []
  in
  aux Gamma.Index.empty Gamma.Constr.empty
