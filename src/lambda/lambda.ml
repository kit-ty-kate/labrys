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
        Abs (name, aux params (pred n))
  in
  aux [] n

let create_fresh_name freshn =
  let name = fmt ".@fresh.%d" freshn in
  Ident.Name.local_create ~loc:Builtins.unknown_loc name

let rec of_results freshn mapn var m =
  let aux (wildcards, t) =
    let remove acc (_, name) = GammaMap.Value.remove name acc in
    let mapn = List.fold_left remove mapn wildcards in
    let t = of_typed_term freshn mapn t in
    let aux t (idx, name) =
      let rec aux freshn var = function
        | PatternMatrix.VLeaf ->
            Val var
        | PatternMatrix.VNode (idx, xs) ->
            let name = create_fresh_name freshn in
            let freshn = succ freshn in
            let t = aux freshn name xs in
            (* NOTE: "succ idx" is here because variants' first element is
                     taken by its tag. *)
            Let (name, RecordGet (var, succ idx), t)
      in
      Let (name, aux freshn var idx, t)
    in
    List.fold_left aux t wildcards
  in
  List.map aux m

and of_args f freshn mapn args =
  let args =
    let aux (acc, freshn) t =
      let name = create_fresh_name freshn in
      let t = of_typed_term freshn mapn t in
      ((name, t) :: acc, succ freshn)
    in
    let (args, _) =
      List.fold_left aux ([], freshn) args
    in
    List.rev args
  in
  let rec aux names = function
    | [] -> f (List.rev names)
    | (name, t)::args -> Let (name, t, aux (name :: names) args)
  in
  aux [] args

and of_typed_term freshn mapn = function
  | UntypedTree.Abs (name, t) ->
      let mapn = GammaMap.Value.remove name mapn in
      let t = of_typed_term freshn mapn t in
      Abs (name, t)
  | UntypedTree.App (f, x) ->
      let f = of_typed_term (succ freshn) mapn f in
      let x = of_typed_term freshn mapn x in
      let name_x = create_fresh_name freshn in
      let freshn = succ freshn in
      let name_f = create_fresh_name freshn in
      Let (name_x, x, Let (name_f, f, App (name_f, name_x)))
  | UntypedTree.Val name ->
      let name = match GammaMap.Value.find_opt name mapn with
        | Some x -> x
        | None -> name
      in
      Val name
  | UntypedTree.Var (idx, len) ->
      create_dyn_functions
        (fun params -> Datatype (Some idx, params))
        len
  | UntypedTree.PatternMatching (t, results, default, patterns) ->
      let t = of_typed_term freshn mapn t in
      let name = create_fresh_name freshn in
      let freshn = succ freshn in
      let results = of_results freshn mapn name results in
      let patterns = of_patterns patterns in
      let default = of_typed_term freshn mapn default in
      let pat = PatternMatching (name, results, default, patterns) in
      Let (name, t, pat)
  | UntypedTree.Try (t, (name, t')) ->
      let t = of_typed_term freshn mapn t in
      let t' = of_typed_term freshn mapn t' in
      Try (t, (name, t'))
  | UntypedTree.Let (name, t, xs) ->
      let t = of_typed_term freshn mapn t in
      let mapn = GammaMap.Value.remove name mapn in
      let xs = of_typed_term freshn mapn xs in
      Let (name, t, xs)
  | UntypedTree.Rec (name, t) ->
      let mapn = GammaMap.Value.remove name mapn in
      let t = of_typed_term freshn mapn t in
      Rec (name, t)
  | UntypedTree.Fail (name, args) ->
      of_args (fun names -> Fail (name, names)) freshn mapn args
  | UntypedTree.RecordGet (t, n) ->
      let name = create_fresh_name freshn in
      let t = of_typed_term freshn mapn t in
      Let (name, t, RecordGet (name, n))
  | UntypedTree.RecordCreate fields ->
      of_args (fun names -> (Datatype (None, names))) freshn mapn fields
  | UntypedTree.Const const ->
      Const const
  | UntypedTree.Unreachable ->
      Unreachable
  | UntypedTree.Reraise e ->
      Reraise e

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

let create_dyn_functions cname (ret, args) =
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
            let t =
              aux ((ty, name) :: args) (succ n) xs
            in
            Abs (name, t)
        | [] ->
            CallForeign (cname, ret, List.rev args)
      in
      let name = create_name 0 in
      let t = aux [(ty, name)] 1 args in
      Abs (name, t)

let rec of_typed_tree names mapn = function
  | UntypedTree.Value (name, t) :: xs ->
      let t = of_typed_term 0 mapn t in
      let (name, names, mapn, linkage) = get_name_and_linkage name names mapn in
      let xs = of_typed_tree names mapn xs in
      Value (name, t, linkage) :: xs
  | UntypedTree.Foreign (cname, name, ty) :: xs ->
      let (name, names, mapn, linkage) = get_name_and_linkage name names mapn in
      let xs = of_typed_tree names mapn xs in
      Value (name, create_dyn_functions cname ty, linkage) :: xs
  | UntypedTree.Exception name :: xs ->
      let xs = of_typed_tree names mapn xs in
      Exception name :: xs
  | [] ->
      []

let of_typed_tree top =
  let add name names = GammaMap.Value.modify_def (-1) name succ names in
  let aux names = function
    | UntypedTree.Value (name, _) -> add name names
    | UntypedTree.Foreign (_, name, _) -> add name names
    | UntypedTree.Exception _ -> names
  in
  let names = List.fold_left aux GammaMap.Value.empty top in
  of_typed_tree names GammaMap.Value.empty top
