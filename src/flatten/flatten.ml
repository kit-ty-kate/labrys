(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

open FlattenTree

type value =
  | Name of name
  | Abstr of (name * t)

let rename gamma name =
  match LIdent.Map.find name gamma with
  | Some (Name name) -> name
  | Some (Abstr _) | None -> name

let rec propagate' gamma = function
  | Abs (name, t) ->
      let t = propagate gamma t in
      ([], Abs (name, t))
  | Rec (name, t) ->
      let (lets, t) = propagate' gamma t in
      (lets, Rec (name, t))
  | App (x, y) ->
      let x = rename gamma x in
      let y = rename gamma y in
      begin match LIdent.Map.find x gamma with
      | Some (Abstr (name, (lets, t))) -> propagate gamma ((name, Val y) :: lets, t)
      | Some (Name _) | None -> ([], App (x, y))
      end
  | Val name ->
      let name = rename gamma name in
      ([], Val name)
  | Datatype (idx, args) ->
      ([], Datatype (idx, args))
  | CallForeign (name, ret, args) ->
      ([], CallForeign (name, ret, args))
  | PatternMatching (name, branches, default, tree) ->
      let name = rename gamma name in
      let branches = List.map (propagate gamma) branches in
      let default = propagate gamma default in
      ([], PatternMatching (name, branches, default, tree))
  | Fail (name, args) ->
      ([], Fail (name, args))
  | Try (t, (name, t')) ->
      let t = propagate gamma t in
      let t' = propagate gamma t' in
      ([], Try (t, (name, t')))
  | RecordGet (name, idx) ->
      let name = rename gamma name in
      ([], RecordGet (name, idx))
  | Const c ->
      ([], Const c)
  | Unreachable ->
      ([], Unreachable)
  | Reraise name ->
      ([], Reraise name)

and propagate gamma (lets, t) =
  let rec aux gamma = function
    | (name, x)::xs ->
        let (lets1, x) = propagate' gamma x in
        let gamma = match x with
          | Val x -> LIdent.Map.add name (Name x) gamma
          | Abs x -> LIdent.Map.add name (Abstr x) gamma
          | _ -> gamma
        in
        let (gamma, lets2) = aux gamma xs in
        (gamma, lets1 @ [(name, x)] @ lets2)
    | [] ->
        (gamma, [])
  in
  let (gamma, lets) = aux gamma lets in
  let (lets_t, t) = propagate' gamma t in
  (lets @ lets_t, t)

let rec of_term = function
  | LambdaTree.Abs (name, t) ->
      let t = of_term t in
      ([], Abs (name, t))
  | LambdaTree.App (x, y) ->
      ([], App (x, y))
  | LambdaTree.Val name ->
      ([], Val name)
  | LambdaTree.Datatype (idx, args) ->
      ([], Datatype (idx, args))
  | LambdaTree.CallForeign (name, ret, args) ->
      ([], CallForeign (name, ret, args))
  | LambdaTree.PatternMatching (name, branches, default, tree) ->
      let branches = List.map of_term branches in
      let default = of_term default in
      ([], PatternMatching (name, branches, default, tree))
  | LambdaTree.Let (name, x, t) ->
      let (lets_x, x) = of_term x in
      let (lets_t, t) = of_term t in
      (lets_x @ [(name, x)] @ lets_t, t)
  | LambdaTree.LetRec (name, x, t) ->
      let (lets_x, x) = of_term x in
      let (lets_t, t) = of_term t in
      (lets_x @ [(name, Rec (name, x))] @ lets_t, t)
  | LambdaTree.Fail (name, args) ->
      ([], Fail (name, args))
  | LambdaTree.Try (t, (name, t')) ->
      let t = of_term t in
      let t' = of_term t' in
      ([], Try (t, (name, t')))
  | LambdaTree.RecordGet (name, idx) ->
      ([], RecordGet (name, idx))
  | LambdaTree.Const c ->
      ([], Const c)
  | LambdaTree.Unreachable ->
      ([], Unreachable)
  | LambdaTree.Reraise name ->
      ([], Reraise name)

let of_lambda_tree top =
  let aux = function
    | LambdaTree.Value (name, t, linkage) ->
        let t = propagate LIdent.Map.empty (of_term t) in
        Value (name, t, linkage)
    | LambdaTree.Exception name ->
        Exception name
  in
  List.map aux top
