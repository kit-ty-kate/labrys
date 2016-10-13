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

open Containers
open Monomorphic.None

open LambdaTree

module Set = GammaSet.Value

let fmt = Printf.sprintf

let rec of_patterns' f = function
  | Pattern.Leaf label ->
      Leaf label
  | Pattern.Node (var, cases) ->
      let aux (constr, tree) = (f constr, of_patterns' f tree) in
      let cases = List.map aux cases in
      Node (var, cases)

let of_patterns = function
  | Pattern.Idx pat -> IdxTree (of_patterns' snd pat)
  | Pattern.Ptr pat -> PtrTree (of_patterns' Fun.id pat)

let create_dyn_functions f n =
  let rec aux params = function
    | 0 ->
        f params
    | n ->
        let name =
          Ident.Name.local_create
            ~loc:Builtins.unknown_loc
            (string_of_int n)
        in
        let params = name :: params in
        let (t, used_vars) = aux params (pred n) in
        let used_vars = Set.remove name used_vars in
        (Abs (name, used_vars, t), used_vars)
  in
  let (t, used_vars) = aux [] n in
  if Int.(Set.cardinal used_vars <> 0) then
    assert false;
  (t, Set.empty)

let create_fresh_name freshn =
  let name = fmt ".@fresh.%d" freshn in
  Ident.Name.local_create ~loc:Builtins.unknown_loc name

let rec of_results freshn mapn m =
  let aux (acc, used_vars_acc) (wildcards, t) =
    let remove acc (_, name) = GammaMap.Value.remove name acc in
    let mapn = List.fold_left remove mapn wildcards in
    let (t, used_vars) = of_typed_term freshn mapn t in
    let remove acc (_, name) = Set.remove name acc in
    let used_vars = List.fold_left remove used_vars wildcards in
    ((wildcards, t) :: acc, Set.union used_vars used_vars_acc)
  in
  let (a, b) = List.fold_left aux ([], Set.empty) m in
  (List.rev a, b)

and of_args f freshn mapn args =
  let (args, used_vars) =
    let aux (acc, used_vars_acc, freshn) t =
      let name = create_fresh_name freshn in
      let (t, used_vars) = of_typed_term freshn mapn t in
      ((name, t) :: acc, Set.union used_vars used_vars_acc, succ freshn)
    in
    let (args, used_vars, _) =
      List.fold_left aux ([], Set.empty, freshn) args
    in
    (List.rev args, used_vars)
  in
  let rec aux names = function
    | [] -> f (List.rev names)
    | (name, t)::args -> Let (name, NonRec, t, aux (name :: names) args)
  in
  (aux [] args, used_vars)

and of_typed_term freshn mapn = function
  | UntypedTree.Abs (name, t) ->
      let mapn = GammaMap.Value.remove name mapn in
      let (t, used_vars) = of_typed_term freshn mapn t in
      let used_vars = Set.remove name used_vars in
      (Abs (name, used_vars, t), used_vars)
  | UntypedTree.App (f, x) ->
      let (f, used_vars1) = of_typed_term (succ freshn) mapn f in
      let (x, used_vars2) = of_typed_term freshn mapn x in
      let name = create_fresh_name freshn in
      (Let (name, NonRec, x, App (f, name)), Set.union used_vars1 used_vars2)
  | UntypedTree.Val name ->
      let name = match GammaMap.Value.find_opt name mapn with
        | Some x -> x
        | None -> name
      in
      (Val name, Set.singleton name)
  | UntypedTree.Var (idx, len) ->
      create_dyn_functions
        (fun params -> (Datatype (Some idx, params), Set.of_list params))
        len
  | UntypedTree.PatternMatching (t, results, default, patterns) ->
      let (t, used_vars1) = of_typed_term freshn mapn t in
      let name = create_fresh_name freshn in
      let freshn = succ freshn in
      let (results, used_vars2) = of_results freshn mapn results in
      let patterns = of_patterns patterns in
      let (default, used_vars3) = of_typed_term freshn mapn default in
      let used_vars = Set.union3 used_vars1 used_vars2 used_vars3 in
      let pat = PatternMatching (name, results, default, patterns) in
      (Let (name, NonRec, t, pat), used_vars)
  | UntypedTree.Try (t, (name, t')) ->
      let (t, used_vars1) = of_typed_term freshn mapn t in
      let (t', used_vars2) = of_typed_term freshn mapn t' in
      let used_vars = Set.union used_vars1 (Set.remove name used_vars2) in
      (Try (t, (name, t')), used_vars)
  | UntypedTree.Let ((name, UntypedTree.NonRec, t), xs) ->
      let (t, used_vars1) = of_typed_term freshn mapn t in
      let mapn = GammaMap.Value.remove name mapn in
      let (xs, used_vars2) = of_typed_term freshn mapn xs in
      let used_vars = Set.union used_vars1 (Set.remove name used_vars2) in
      (Let (name, NonRec, t, xs), used_vars)
  | UntypedTree.Let ((name, UntypedTree.Rec, t), xs) ->
      let mapn = GammaMap.Value.remove name mapn in
      let (t, used_vars1) = of_typed_term freshn mapn t in
      let (xs, used_vars2) = of_typed_term freshn mapn xs in
      let used_vars =
        Set.union (Set.remove name used_vars1) (Set.remove name used_vars2)
      in
      (Let (name, Rec, t, xs), used_vars)
  | UntypedTree.Fail (name, args) ->
      of_args (fun names -> (Fail (name, names))) freshn mapn args
  | UntypedTree.RecordGet (t, n) ->
      let name = create_fresh_name freshn in
      let (t, used_vars) = of_typed_term freshn mapn t in
      (Let (name, NonRec, t, RecordGet (name, n)), used_vars)
  | UntypedTree.RecordCreate fields ->
      of_args (fun names -> (Datatype (None, names))) freshn mapn fields
  | UntypedTree.Const const ->
      (Const const, Set.empty)
  | UntypedTree.Unreachable ->
      (Unreachable, Set.empty)
  | UntypedTree.Reraise e ->
      (Reraise e, Set.singleton e)

let get_name_and_linkage name' names mapn =
  match GammaMap.Value.find_opt name' names with
  | Some 0
  | None ->
      let mapn = GammaMap.Value.add name' name' mapn in
      (name', names, mapn, Public)
  | Some n ->
      let name = Ident.Name.unique name' n in
      let names = GammaMap.Value.modify_def (-1) name' pred names in
      let mapn = GammaMap.Value.add name' name mapn in
      (name, names, mapn, Private)

let of_ret_type mapn = function
  | UntypedTree.Void t ->
      let (t, used_vars) = of_typed_term 0 mapn t in
      (Void t, used_vars)
  | UntypedTree.Alloc ty ->
      (Alloc ty, Set.empty)

let create_dyn_functions mapn cname (ret, args) =
  let create_name n =
    Ident.Name.local_create ~loc:Builtins.unknown_loc (string_of_int n)
  in
  match args with
  | [] ->
      (* TODO: See TypeChecker.get_foreign_type *)
      assert false
  | ty::args ->
      let rec aux args n = function
        | ty::xs ->
            let name = create_name n in
            let (t, used_vars) =
              aux ((ty, name) :: args) (succ n) xs
            in
            let used_vars = Set.remove name used_vars in
            (Abs (name, used_vars, t), used_vars)
        | [] ->
            let (ret, used_vars) = of_ret_type mapn ret in
            let used_vars =
              List.fold_right (fun (_, name) -> Set.add name) args used_vars
            in
            (CallForeign (cname, ret, List.rev args), used_vars)
      in
      let name = create_name 0 in
      let (t, used_vars) = aux [(ty, name)] 1 args in
      let used_vars = Set.remove name used_vars in
      if Int.(Set.cardinal used_vars <> 0) then
        assert false;
      Abs (name, Set.empty, t)

let rec of_typed_tree names mapn = function
  | UntypedTree.Value (name, UntypedTree.Rec, t) :: xs  ->
      let (name, names, mapn, linkage) = get_name_and_linkage name names mapn in
      let (t, _) = of_typed_term 0 mapn t in
      let xs = of_typed_tree names mapn xs in
      Value (name, t, linkage) :: xs
  | UntypedTree.Value (name, UntypedTree.NonRec, t) :: xs ->
      let (t, _) = of_typed_term 0 mapn t in
      let (name, names, mapn, linkage) = get_name_and_linkage name names mapn in
      let xs = of_typed_tree names mapn xs in
      Value (name, t, linkage) :: xs
  | UntypedTree.Foreign (cname, name, ty) :: xs ->
      let (name, names, mapn, linkage) = get_name_and_linkage name names mapn in
      let xs = of_typed_tree names mapn xs in
      Value (name, create_dyn_functions mapn cname ty, linkage) :: xs
  | UntypedTree.Exception name :: xs ->
      let xs = of_typed_tree names mapn xs in
      Exception name :: xs
  | [] ->
      []

let of_typed_tree top =
  let add name names = GammaMap.Value.modify_def (-1) name succ names in
  let aux names = function
    | UntypedTree.Value (name, _, _) -> add name names
    | UntypedTree.Foreign (_, name, _) -> add name names
    | UntypedTree.Exception _ -> names
  in
  let names = List.fold_left aux GammaMap.Value.empty top in
  of_typed_tree names GammaMap.Value.empty top
