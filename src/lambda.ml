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

let rec of_results mapn m =
  let aux (acc, used_vars_acc) (wildcards, t) =
    let remove acc (_, name) = GammaMap.Value.remove name acc in
    let mapn = List.fold_left remove mapn wildcards in
    let (t, used_vars) = of_typed_term mapn t in
    let remove acc (_, name) = Set.remove name acc in
    let used_vars = List.fold_left remove used_vars wildcards in
    ((wildcards, t) :: acc, Set.union used_vars used_vars_acc)
  in
  let (a, b) = List.fold_left aux ([], Set.empty) m in
  (List.rev a, b)

and of_branches mapn branches =
  let aux (acc, used_vars_acc) ((name, args), t) =
    let remove acc name = GammaMap.Value.remove name acc in
    let mapn = List.fold_left remove mapn args in
    let (t, used_vars) = of_typed_term mapn t in
    let remove acc name = Set.remove name acc in
    let used_vars = List.fold_left remove used_vars args in
    (((name, args), t) :: acc, Set.union used_vars used_vars_acc)
  in
  let (a, b) = List.fold_left aux ([], Set.empty) branches in
  (List.rev a, b)

and of_typed_term mapn = function
  | TypedTree.Abs (name, with_exn, t) ->
      let mapn = GammaMap.Value.remove name mapn in
      let (t, used_vars) = of_typed_term mapn t in
      let used_vars = Set.remove name used_vars in
      (Abs (name, with_exn, used_vars, t), used_vars)
  | TypedTree.TApp t
  | TypedTree.TAbs t ->
      of_typed_term mapn t
  | TypedTree.App (f, with_exn, x) ->
      let (f, used_vars1) = of_typed_term mapn f in
      let (x, used_vars2) = of_typed_term mapn x in
      (App (f, with_exn, x), Set.union used_vars1 used_vars2)
  | TypedTree.Val name ->
      let name = match GammaMap.Value.find name mapn with
        | Some x -> x
        | None -> name
      in
      (Val name, Set.singleton name)
  | TypedTree.PatternMatching (t, results, patterns) ->
      let (t, used_vars1) = of_typed_term mapn t in
      let (results, used_vars2) = of_results mapn results in
      let patterns = of_patterns patterns in
      (PatternMatching (t, results, patterns), Set.union used_vars1 used_vars2)
  | TypedTree.Try (t, with_exn, branches) ->
      let (t, used_vars1) = of_typed_term mapn t in
      let (branches, used_vars2) = of_branches mapn branches in
      (Try (t, with_exn, branches), Set.union used_vars1 used_vars2)
  | TypedTree.Let (name, t, xs) ->
      let (t, used_vars1) = of_typed_term mapn t in
      let mapn = GammaMap.Value.remove name mapn in
      let (xs, used_vars2) = of_typed_term mapn xs in
      let used_vars = Set.union used_vars1 (Set.remove name used_vars2) in
      (Let (name, t, xs), used_vars)
  | TypedTree.LetRec (name, t, xs) ->
      let mapn = GammaMap.Value.remove name mapn in
      let (t, used_vars1) = of_typed_term mapn t in
      let (xs, used_vars2) = of_typed_term mapn xs in
      let used_vars =
        Set.union (Set.remove name used_vars1) (Set.remove name used_vars2)
      in
      (LetRec (name, t, xs), used_vars)
  | TypedTree.Fail (name, args) ->
      let (args, used_vars) =
        let aux (acc, used_vars_acc) t =
          let (t, used_vars) = of_typed_term mapn t in
          (t :: acc, Set.union used_vars used_vars_acc)
        in
        List.fold_left aux ([], Set.empty) args
      in
      (Fail (name, args), used_vars)

let of_typed_variant acc i (`Variant (name, ty_size, linkage)) =
  let variant =
    let rec aux params = function
      | 0 ->
          (Variant (i, List.rev params), Set.of_list params)
      | n ->
          let name = Ident.Name.of_list [string_of_int n] in
          let params = name :: params in
          let (t, used_vars) = aux params (pred n) in
          let used_vars = Set.remove name used_vars in
          (Abs (name, false, used_vars, t), used_vars)
    in
    match ty_size with
    | 0 -> ConstVariant (name, i, linkage)
    | n ->
        let name_param = Ident.Name.of_list [string_of_int n] in
        let params = [name_param] in
        let (t, used_vars) = aux params (pred n) in
        let used_vars = Set.remove name_param used_vars in
        if Int.(Set.cardinal used_vars <> 0) then
          assert false;
        Function (name, (name_param, false, t), linkage)
  in
  variant :: acc

let rec of_typed_tree = function
  | `RecValue (name, TypedTree.Abs (name', with_exn, t), linkage, mapn) :: xs
  | `Value (name, TypedTree.Abs (name', with_exn, t), linkage, mapn) :: xs ->
      let (t, _) = of_typed_term mapn t in
      Function (name, (name', with_exn, t), linkage) :: of_typed_tree xs
  | `Value (name, t, linkage, mapn) :: xs ->
      let (t, _) = of_typed_term mapn t in
      Value (name, t, linkage) :: of_typed_tree xs
  | `RecValue (name, t, linkage, mapn) :: xs ->
      let (t, _) = of_typed_term mapn t in
      Value (name, t, linkage) :: of_typed_tree xs
  | `Binding (name, value, linkage) :: xs ->
      Binding (name, value, linkage) :: of_typed_tree xs
  | `Datatype variants :: xs ->
      let variants = List.fold_lefti of_typed_variant [] variants in
      let variants = List.rev variants in
      variants @ of_typed_tree xs
  | `Exception name :: xs ->
      Exception name :: of_typed_tree xs
  | [] ->
      []

let get_linkage b = if b then Private else Public

let of_typed_tree top =
  let aux topx (acc, names, mapn) = match topx with
    | TypedTree.Value (name', t) ->
        let (name, names, changed) = Ident.Name.unique name' names in
        let mapn = GammaMap.Value.add name' name mapn in
        (`Value (name, t, get_linkage changed, mapn) :: acc, names, mapn)
    | TypedTree.RecValue (name', t) ->
        let (name, names, changed) = Ident.Name.unique name' names in
        let mapn = GammaMap.Value.add name' name mapn in
        (`RecValue (name, t, get_linkage changed, mapn) :: acc, names, mapn)
    | TypedTree.Binding (name', value) ->
        let (name, names, changed) = Ident.Name.unique name' names in
        let mapn = GammaMap.Value.add name' name mapn in
        (`Binding (name, value, get_linkage changed) :: acc, names, mapn)
    | TypedTree.Datatype variants ->
        let aux_variants (variants, names, mapn) (TypedTree.Variant (name', index)) =
          let (name, names, changed) = Ident.Name.unique name' names in
          let mapn = GammaMap.Value.add name' name mapn in
          (`Variant (name, index, get_linkage changed) :: variants, names, mapn)
        in
        let (variants, names, mapn) =
          List.fold_left aux_variants ([], names, mapn) variants
        in
        let variants = List.rev variants in
        (`Datatype variants :: acc, names, mapn)
    | TypedTree.Exception name ->
        (`Exception name :: acc, names, mapn)
  in
  let (top, _, _) = List.fold_right aux top ([], Utils.StrMap.empty, GammaMap.Value.empty) in
  of_typed_tree top
