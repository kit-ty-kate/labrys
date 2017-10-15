(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open FlattenTree

type value =
  | Name of name
  | Abstr of (name * t)

let rename env name =
  match LIdent.Map.find name env with
  | Some (Name name) -> name
  | Some (Abstr _) | None -> name

let rec propagate' env = function
  | Abs (name, t) ->
      let t = propagate env t in
      ([], Abs (name, t))
  | Rec (name, t) ->
      let (lets, t) = propagate' env t in
      (lets, Rec (name, t))
  | App (x, y) ->
      let x = rename env x in
      let y = rename env y in
      begin match LIdent.Map.find x env with
      | Some (Abstr (name, (lets, t))) -> propagate env ((name, Val y) :: lets, t)
      | Some (Name _) | None -> ([], App (x, y))
      end
  | Val name ->
      let name = rename env name in
      ([], Val name)
  | Datatype (idx, args) ->
      ([], Datatype (idx, args))
  | CallForeign (name, ret, args) ->
      ([], CallForeign (name, ret, args))
  | PatternMatching (name, branches, tree) ->
      let name = rename env name in
      let branches = List.map (propagate_branch env) branches in
      ([], PatternMatching (name, branches, tree))
  | Fail name ->
      ([], Fail name)
  | Try (t, (name, t')) ->
      let t = propagate env t in
      let t' = propagate env t' in
      ([], Try (t, (name, t')))
  | RecordGet (name, idx) ->
      let name = rename env name in
      ([], RecordGet (name, idx))
  | Const c ->
      ([], Const c)
  | Unreachable ->
      ([], Unreachable)

and propagate_branch env (vars, t) = (vars, propagate env t)

and propagate env (lets, t) =
  let rec aux env = function
    | (name, x)::xs ->
        let (lets1, x) = propagate' env x in
        let env = match x with
          | Val x -> LIdent.Map.add name (Name x) env
          | Abs x -> LIdent.Map.add name (Abstr x) env
          | _ -> env
        in
        let (env, lets2) = aux env xs in
        (env, lets1 @ [(name, x)] @ lets2)
    | [] ->
        (env, [])
  in
  let (env, lets) = aux env lets in
  let (lets_t, t) = propagate' env t in
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
  | LambdaTree.PatternMatching (name, branches, tree) ->
      let branches = List.map of_branch branches in
      ([], PatternMatching (name, branches, tree))
  | LambdaTree.Let (name, x, t) ->
      let (lets_x, x) = of_term x in
      let (lets_t, t) = of_term t in
      (lets_x @ [(name, x)] @ lets_t, t)
  | LambdaTree.LetRec (name, x, t) ->
      let (lets_x, x) = of_term x in
      let (lets_t, t) = of_term t in
      (lets_x @ [(name, Rec (name, x))] @ lets_t, t)
  | LambdaTree.Fail name ->
      ([], Fail name)
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

and of_branch (vars, t) = (vars, of_term t)

let of_lambda_tree top =
  let aux = function
    | LambdaTree.Value (name, t, linkage) ->
        let t = propagate LIdent.Map.empty (of_term t) in
        Value (name, t, linkage)
    | LambdaTree.Exception name ->
        Exception name
  in
  List.map aux top
