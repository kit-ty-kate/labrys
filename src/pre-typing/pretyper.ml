(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

open PretypedTree

module ForbiddenEnv = Ident.Name.Set

let ty_to_effects ty =
  (fst ty, [ty])

let rec get_rec_ty ~name (loc, t) = match t with
  | Annot (_, ty) ->
      ty
  | Let (_, _, t)
  | LetRec (_, _, _, t) ->
      get_rec_ty ~name t
  | Abs ((name, ty), t) ->
      let (ty', eff) = get_rec_ty ~name t in
      let eff = Option.map ty_to_effects eff in
      ((loc, Fun (ty, eff, ty')), None)
  | TAbs ((tname, k), t) ->
      let (ty', eff) = get_rec_ty ~name t in
      ((loc, Forall ((tname, k), ty')), eff)
  | CAbs ((_, cl), t) ->
      let (ty', eff) = get_rec_ty ~name t in
      let eff = Option.map ty_to_effects eff in
      ((loc, TyClass (cl, eff, ty')), None)
  | App _ | TApp _ | CApp _
  | Val _ | Var _ | Const _
  | PatternMatching _
  | Fail _ | Try _ ->
      let loc = Ident.Name.loc name in
      Err.fail ~loc "Recursive functions must have explicit return types"

let rec pretype_abs ~last_let forbidden_env t =
  let forbidden_env = match last_let with
    | Some last_let -> ForbiddenEnv.remove last_let forbidden_env
    | None -> forbidden_env
  in
  pretype_term forbidden_env t

and pretype_term ?last_let forbidden_env = function
  | (loc, DesugaredTree.Abs (arg, t)) ->
      let t = pretype_abs ~last_let forbidden_env t in
      (loc, Abs (arg, t))
  | (loc, DesugaredTree.TAbs (arg, t)) ->
      let t = pretype_term forbidden_env t in
      (loc, TAbs (arg, t))
  | (loc, DesugaredTree.CAbs (arg, t)) ->
      let t = pretype_abs ~last_let forbidden_env t in
      (loc, CAbs (arg, t))
  | (loc, DesugaredTree.App (f, x)) ->
      let f = pretype_term forbidden_env f in
      let x = pretype_term forbidden_env x in
      (loc, App (f, x))
  | (loc, DesugaredTree.TApp (f, ty_x)) ->
      let f = pretype_term forbidden_env f in
      (loc, TApp (f, ty_x))
  | (loc, DesugaredTree.CApp (f, x)) ->
      let f = pretype_term forbidden_env f in
      (loc, CApp (f, x))
  | (loc, DesugaredTree.Val name) ->
      if ForbiddenEnv.mem name forbidden_env then
        Err.fail ~loc "This recursive value cannot be used here"
      else
        (loc, Val name)
  | (loc, DesugaredTree.Var name) ->
      (loc, Var name)
  | (loc, DesugaredTree.PatternMatching (t, patterns)) ->
      let t = pretype_term forbidden_env t in
      let patterns =
        let aux (pat, b) = (pat, pretype_term forbidden_env b) in
        List.map aux patterns
      in
      (loc, PatternMatching (t, patterns))
  | (loc, DesugaredTree.Let (name, t, xs)) ->
      let t = pretype_term forbidden_env t in
      let xs = pretype_term ?last_let forbidden_env xs in
      (loc, Let (name, t, xs))
  | (loc, DesugaredTree.LetRec (name, t, xs)) ->
      let t =
        let forbidden_env = ForbiddenEnv.add name forbidden_env in
        pretype_term ~last_let:name forbidden_env t
      in
      let ty = get_rec_ty ~name t in
      let xs = pretype_term ?last_let forbidden_env xs in
      (loc, LetRec (name, fst ty, (loc, Annot (t, ty)), xs))
  | (loc, DesugaredTree.Fail (ty, t)) ->
      let t = pretype_term forbidden_env t in
      (loc, Fail (ty, t))
  | (loc, DesugaredTree.Try (e, branches)) ->
      let e = pretype_term forbidden_env e in
      let branches =
        let aux (arg, b) = (arg, pretype_term forbidden_env b) in
        List.map aux branches
      in
      (loc, Try (e, branches))
  | (loc, DesugaredTree.Annot (t, ty)) ->
      let t = pretype_term ?last_let forbidden_env t in
      (loc, Annot (t, ty))
  | (loc, DesugaredTree.Const const) ->
      (loc, Const const)

let forbidden_env = ForbiddenEnv.empty

let pretype_top = function
  | DesugaredTree.Value (name, t) ->
      let t = pretype_term forbidden_env t in
      Value (name, t)
  | DesugaredTree.Type ty ->
      Type ty
  | DesugaredTree.Foreign foreign ->
      Foreign foreign
  | DesugaredTree.Datatype dtype ->
      Datatype dtype
  | DesugaredTree.Exception exn ->
      Exception exn
  | DesugaredTree.Class cl ->
      Class cl
  | DesugaredTree.Instance (tyclass, name, values) ->
      let values =
        let aux (name, t) = (name, pretype_term forbidden_env t) in
        List.map aux values
      in
      Instance (tyclass, name, values)

let pretype top =
  List.map pretype_top top

let pretype_interface =
  Fun.id
