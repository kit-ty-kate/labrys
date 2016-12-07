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

let gamma_add name gamma =
  let id = LIdent.create (Ident.Name.to_string name) in
  (id, GammaMap.Value.add name id gamma)

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
        let name = LIdent.create (string_of_int n) in
        let params = name :: params in
        Abs (name, aux params (pred n))
  in
  aux [] n

let create_fresh_name () = LIdent.create ".@fresh"

let get_name name gamma =
  match GammaMap.Value.find_opt name gamma with
  | Some x -> x
  | None -> LIdent.create (Ident.Name.to_string name) (* External value *)

let rec of_results gamma var m =
  let aux (wildcards, t) =
    let rec aux gamma t = function
      | (idx, name)::xs ->
          let rec aux' var = function
            | PatternMatrix.VLeaf ->
                Val var
            | PatternMatrix.VNode (idx, xs) ->
                let name = create_fresh_name () in
                let t = aux' name xs in
                (* NOTE: "succ idx" is here because variants' first element is
                   taken by its tag. *)
                Let (name, RecordGet (var, succ idx), t)
          in
          let (name, gamma) = gamma_add name gamma in
          Let (name, aux' var idx, aux gamma t xs)
      | [] ->
          of_typed_term gamma t
    in
    aux gamma t wildcards
  in
  List.map aux m

and of_args gamma f args =
  let args =
    let aux t =
      let name = create_fresh_name () in
      let t = of_typed_term gamma t in
      (name, t)
    in
    List.map aux args
  in
  let rec aux names = function
    | [] -> f (List.rev names)
    | (name, t)::args -> Let (name, t, aux (name :: names) args)
  in
  aux [] args

and of_typed_term gamma = function
  | UntypedTree.Abs (name, t) ->
      let (name, gamma) = gamma_add name gamma in
      let t = of_typed_term gamma t in
      Abs (name, t)
  | UntypedTree.App (f, x) ->
      let f = of_typed_term gamma f in
      let x = of_typed_term gamma x in
      let name_x = create_fresh_name () in
      let name_f = create_fresh_name () in
      Let (name_x, x, Let (name_f, f, App (name_f, name_x)))
  | UntypedTree.Val name ->
      Val (get_name name gamma)
  | UntypedTree.Var (idx, len) ->
      create_dyn_functions
        (fun params -> Datatype (Some idx, params))
        len
  | UntypedTree.PatternMatching (t, results, default, patterns) ->
      let t = of_typed_term gamma t in
      let name = create_fresh_name () in
      let results = of_results gamma name results in
      let patterns = of_patterns patterns in
      let default = of_typed_term gamma default in
      let pat = PatternMatching (name, results, default, patterns) in
      Let (name, t, pat)
  | UntypedTree.Try (t, (name, t')) ->
      let t = of_typed_term gamma t in
      let (name, gamma) = gamma_add name gamma in
      let t' = of_typed_term gamma t' in
      Try (t, (name, t'))
  | UntypedTree.Let (name, UntypedTree.Rec (_, t), xs) ->
      let (name, gamma) = gamma_add name gamma in
      let t = of_typed_term gamma t in
      let xs = of_typed_term gamma xs in
      Let (name, Rec (name, t), xs)
  | UntypedTree.Let (name, t, xs) ->
      let t = of_typed_term gamma t in
      let (name, gamma) = gamma_add name gamma in
      let xs = of_typed_term gamma xs in
      Let (name, t, xs)
  | UntypedTree.Rec _ ->
      assert false
  | UntypedTree.Fail (name, args) ->
      of_args gamma (fun names -> Fail (name, names)) args
  | UntypedTree.RecordGet (t, n) ->
      let name = create_fresh_name () in
      let t = of_typed_term gamma t in
      Let (name, t, RecordGet (name, n))
  | UntypedTree.RecordCreate fields ->
      of_args gamma (fun names -> (Datatype (None, names))) fields
  | UntypedTree.Const const ->
      Const const
  | UntypedTree.Unreachable ->
      Unreachable
  | UntypedTree.Reraise e ->
      Reraise (get_name e gamma)

let create_dyn_functions cname (ret, args) =
  match args with
  | [] ->
      (* TODO: See TypeChecker.get_foreign_type *)
      assert false
  | ty::args ->
      let rec aux args n = function
        | ty::xs ->
            let name = LIdent.create (string_of_int n) in
            let t =
              aux ((ty, name) :: args) (succ n) xs
            in
            Abs (name, t)
        | [] ->
            CallForeign (cname, ret, List.rev args)
      in
      let name = LIdent.create (string_of_int 0) in
      let t = aux [(ty, name)] 1 args in
      Abs (name, t)

let gamma_add mset name gamma =
  let mset = GammaSet.MValue.remove mset name in
  let (name', linkage) = match GammaSet.MValue.count mset name with
    | 0 -> (name, Public)
    | n -> (Ident.Name.unique name n, Private)
  in
  let id = LIdent.create (Ident.Name.to_string name') in
  let gamma = GammaMap.Value.add name id gamma in
  (id, mset, gamma, linkage)

let rec of_typed_tree mset gamma = function
  | UntypedTree.Value (name, UntypedTree.Rec (_, t)) :: xs ->
      let (name, mset, gamma, linkage) = gamma_add mset name gamma in
      let t = of_typed_term gamma t in
      let xs = of_typed_tree mset gamma xs in
      Value (name, Rec (name, t), linkage) :: xs
  | UntypedTree.Value (name, t) :: xs ->
      let t = of_typed_term gamma t in
      let (name, mset, gamma, linkage) = gamma_add mset name gamma in
      let xs = of_typed_tree mset gamma xs in
      Value (name, t, linkage) :: xs
  | UntypedTree.Foreign (cname, name, ty) :: xs ->
      let (name, mset, gamma, linkage) = gamma_add mset name gamma in
      let xs = of_typed_tree mset gamma xs in
      Value (name, create_dyn_functions cname ty, linkage) :: xs
  | UntypedTree.Exception name :: xs ->
      let xs = of_typed_tree mset gamma xs in
      Exception name :: xs
  | [] ->
      []

let rec scan mset = function
  | UntypedTree.Value (name, _) :: xs
  | UntypedTree.Foreign (_, name, _) :: xs ->
      scan (GammaSet.MValue.add mset name) xs
  | UntypedTree.Exception _ :: xs ->
      scan mset xs
  | [] ->
      mset

let of_typed_tree top =
  let mset = scan GammaSet.MValue.empty top in
  let gamma = GammaMap.Value.empty in
  of_typed_tree mset gamma top
