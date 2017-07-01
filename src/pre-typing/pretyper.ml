(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

open PretypedTree

module ForbiddenEnv = GammaSet.Value

let rec get_rec_ty ~name options (loc, t) = match t with
  | Annot (_, ty) ->
      ty
  | Let (_, _, t)
  | LetRec (_, _, _, t) ->
      get_rec_ty ~name options t
  | Abs ((name, ty), t) ->
      let (ty', eff) = get_rec_ty ~name options t in
      ((loc, Desugar.arrow ~loc options ty eff ty'), None)
  | TAbs ((tname, k), t) ->
      let (ty', eff) = get_rec_ty ~name options t in
      ((loc, Forall ((tname, k), ty')), eff)
  | CAbs ((_, cl), t) ->
      let (ty', eff) = get_rec_ty ~name options t in
      let eff = Option.get_or ~default:(Builtins.unknown_loc, []) eff in
      ((loc, TyClass (cl, eff, ty')), None)
  | App _ | TApp _ | CApp _
  | Val _ | Var _ | Const _
  | PatternMatching _
  | Fail _ | Try _ ->
      let loc = Ident.Name.loc name in
      Err.fail ~loc "Recursive functions must have explicit return types"

let rec pretype_abs ~last_let options forbidden_env t =
  let forbidden_env = match last_let with
    | Some last_let -> ForbiddenEnv.remove last_let forbidden_env
    | None -> forbidden_env
  in
  pretype_term options forbidden_env t

and pretype_term ?last_let options forbidden_env = function
  | (loc, DesugaredTree.Abs (arg, t)) ->
      let t = pretype_abs ~last_let options forbidden_env t in
      (loc, Abs (arg, t))
  | (loc, DesugaredTree.TAbs (arg, t)) ->
      let t = pretype_term options forbidden_env t in
      (loc, TAbs (arg, t))
  | (loc, DesugaredTree.CAbs (arg, t)) ->
      let t = pretype_abs ~last_let options forbidden_env t in
      (loc, CAbs (arg, t))
  | (loc, DesugaredTree.App (f, x)) ->
      let f = pretype_term options forbidden_env f in
      let x = pretype_term options forbidden_env x in
      (loc, App (f, x))
  | (loc, DesugaredTree.TApp (f, ty_x)) ->
      let f = pretype_term options forbidden_env f in
      (loc, TApp (f, ty_x))
  | (loc, DesugaredTree.CApp (f, x)) ->
      let f = pretype_term options forbidden_env f in
      (loc, CApp (f, x))
  | (loc, DesugaredTree.Val name) ->
      if ForbiddenEnv.mem name forbidden_env then
        Err.fail ~loc "This recursive value cannot be used here"
      else
        (loc, Val name)
  | (loc, DesugaredTree.Var name) ->
      (loc, Var name)
  | (loc, DesugaredTree.PatternMatching (t, patterns)) ->
      let t = pretype_term options forbidden_env t in
      let patterns =
        let aux (pat, b) = (pat, pretype_term options forbidden_env b) in
        List.map aux patterns
      in
      (loc, PatternMatching (t, patterns))
  | (loc, DesugaredTree.Let (name, t, xs)) ->
      let t = pretype_term options forbidden_env t in
      let xs = pretype_term ?last_let options forbidden_env xs in
      (loc, Let (name, t, xs))
  | (loc, DesugaredTree.LetRec (name, t, xs)) ->
      let t =
        let forbidden_env = ForbiddenEnv.add name forbidden_env in
        pretype_term ~last_let:name options forbidden_env t
      in
      let ty = get_rec_ty ~name options t in
      let xs = pretype_term ?last_let options forbidden_env xs in
      (loc, LetRec (name, fst ty, (loc, Annot (t, ty)), xs))
  | (loc, DesugaredTree.Fail (ty, (exn, args))) ->
      let args = List.map (pretype_term options forbidden_env) args in
      (loc, Fail (ty, (exn, args)))
  | (loc, DesugaredTree.Try (e, branches)) ->
      let e = pretype_term options forbidden_env e in
      let branches =
        let aux (arg, b) = (arg, pretype_term options forbidden_env b) in
        List.map aux branches
      in
      (loc, Try (e, branches))
  | (loc, DesugaredTree.Annot (t, ty)) ->
      let t = pretype_term ?last_let options forbidden_env t in
      (loc, Annot (t, ty))
  | (loc, DesugaredTree.Const const) ->
      (loc, Const const)

let forbidden_env = ForbiddenEnv.empty

let pretype_top options = function
  | DesugaredTree.Value (name, t) ->
      let t = pretype_term options forbidden_env t in
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
        let aux (name, t) = (name, pretype_term options forbidden_env t) in
        List.map aux values
      in
      Instance (tyclass, name, values)

let pretype options top =
  List.map (pretype_top options) top
