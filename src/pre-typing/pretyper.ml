(*
Copyright (c) 2013-2017 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

open PretypedTree

let rec get_rec_ty ~name t = match snd t with
  | Annot (_, (ty, _)) ->
      ty
  | Let (_, _, t)
  | LetRec (_, _, _, t) ->
      get_rec_ty ~name t
  | Abs _ | TAbs _ | CAbs _
  | App _ | TApp _ | CApp _
  | Val _ | Var _ | Const _
  | PatternMatching _
  | Fail _ | Try _ ->
      let loc = Ident.Name.loc name in
      Err.fail ~loc "Recursive functions must have explicit return types"

let rec pretype_abs ~last_let forbidden_env t =
  let forbidden_env = match last_let with
    | Some last_let -> GammaSet.Value.remove last_let forbidden_env
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
      if GammaSet.Value.mem name forbidden_env then
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
        let forbidden_env = GammaSet.Value.add name forbidden_env in
        pretype_term ~last_let:name forbidden_env t
      in
      let ty = get_rec_ty ~name t in
      let xs = pretype_term ?last_let forbidden_env xs in
      (loc, LetRec (name, ty, t, xs))
  | (loc, DesugaredTree.Fail (ty, (exn, args))) ->
      let args = List.map (pretype_term forbidden_env) args in
      (loc, Fail (ty, (exn, args)))
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

let pretype_top = function
  | DesugaredTree.Value (name, t) ->
      let t = pretype_term GammaSet.Value.empty t in
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
        let aux (name, t) = (name, pretype_term GammaSet.Value.empty t) in
        List.map aux values
      in
      Instance (tyclass, name, values)

let pretype top =
  List.map pretype_top top
