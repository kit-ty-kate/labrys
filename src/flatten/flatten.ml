(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open FlattenTree

type value =
  | Name of name
  | Abstr of (name * t)

let rename env name =
  match LIdent.Map.find_opt name env with
  | Some (Name name) -> name
  | Some (Abstr _) | None -> name

let rec propagate' env = function
  | Abs (name, t) ->
      let t = propagate env t in
      ([], Abs (name, t))
  | App (x, y) ->
      let x = rename env x in
      let y = rename env y in
      begin match LIdent.Map.find_opt x env with
      | Some (Abstr (name, (lets, t))) -> propagate env ((name, false, Val y) :: lets, t)
      | Some (Name _) | None -> ([], App (x, y))
      end
  | Val name ->
      let name = rename env name in
      ([], Val name)
  | Datatype (idx, args) ->
      ([], Datatype (idx, args))
  | CallForeign (name, ret, args) ->
      ([], CallForeign (name, ret, args))
  | PatternMatching (name, vars, branches, tree) ->
      let name = rename env name in
      let branches = List.map (propagate env) branches in
      ([], PatternMatching (name, vars, branches, tree))
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

and propagate env (lets, t) =
  let rec aux env = function
    | (name, is_rec, x)::xs ->
        let (lets1, x) = propagate' env x in
        let env = match x with
          | Val x -> LIdent.Map.add name (Name x) env
          | Abs _ when is_rec -> env
          | Abs x -> LIdent.Map.add name (Abstr x) env
          | _ -> env
        in
        let (env, lets2) = aux env xs in
        (env, lets1 @ [(name, is_rec, x)] @ lets2)
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
  | LambdaTree.PatternMatching (name, vars, branches, tree) ->
      let branches = List.map of_term branches in
      ([], PatternMatching (name, vars, branches, tree))
  | LambdaTree.Let (name, is_rec, x, t) ->
      let (lets_x, x) = of_term x in
      let (lets_t, t) = of_term t in
      (lets_x @ [(name, is_rec, x)] @ lets_t, t)
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

let of_lambda_tree top =
  let aux = function
    | LambdaTree.Value (name, t, linkage) ->
        let t = propagate LIdent.Map.empty (of_term t) in
        Value (name, t, linkage)
    | LambdaTree.Exception name ->
        Exception name
  in
  List.map aux top
