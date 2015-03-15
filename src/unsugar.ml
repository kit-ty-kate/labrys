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

open UnsugaredTree

let unsugar_kind = Option.default Kinds.Star

let rec unsugar_ty =
  let unsugar_forall ~loc ty args =
    let rec aux = function
      | ParseTree.Eff name :: xs -> (loc, ForallEff (name, aux xs))
      | ParseTree.Typ (name, k) :: xs -> (loc, Forall ((name, unsugar_kind k), aux xs))
      | [] -> unsugar_ty ty
    in
    if List.is_empty args then
      assert false;
    aux args
  in
  let unsugar_absOnTy ~loc ty args =
    let rec aux = function
      | (name, k) :: xs -> (loc, AbsOnTy ((name, unsugar_kind k), aux xs))
      | [] -> unsugar_ty ty
    in
    if List.is_empty args then
      assert false;
    aux args
  in
  function
  | (loc, ParseTree.Fun (x, eff, y)) ->
      (loc, Fun (unsugar_ty x, eff, unsugar_ty y))
  | (loc, ParseTree.Ty name) ->
      (loc, Ty name)
  | (loc, ParseTree.Forall (args, ty)) ->
      unsugar_forall ~loc ty args
  | (loc, ParseTree.AbsOnTy (args, ty)) ->
      unsugar_absOnTy ~loc ty args
  | (loc, ParseTree.AppOnTy (x, y)) ->
      (loc, AppOnTy (unsugar_ty x, unsugar_ty y))

let rec unsugar_pattern_arg = function
  | ParseTree.PVal pattern -> PVal (unsugar_pattern pattern)
  | ParseTree.PTy ty -> PTy (unsugar_ty ty)

and unsugar_pattern = function
  | ParseTree.TyConstr (loc, name, args) ->
      TyConstr (loc, name, List.map unsugar_pattern_arg args)
  | ParseTree.Any name ->
      Any name

let rec unsugar_pat (pattern, t) =
  let pattern = match pattern with
    | ParseTree.TyConstr (loc, name, args) ->
        TyConstr (loc, name, List.map unsugar_pattern_arg args)
    | ParseTree.Any name ->
        Any name
  in
  (pattern, unsugar_t t)

and unsugar_try_pattern (pattern, t) = (pattern, unsugar_t t)

and unsugar_t = function
  | (_, ParseTree.Abs (args, t)) ->
      if List.is_empty args then
        assert false;
      snd (unsugar_args args None t)
  | (loc, ParseTree.App (f, x)) ->
      (loc, App (unsugar_t f, unsugar_t x))
  | (loc, ParseTree.TApp (t, ty)) ->
      (loc, TApp (unsugar_t t, unsugar_ty ty))
  | (loc, ParseTree.EApp (t, eff)) ->
      (loc, EApp (unsugar_t t, eff))
  | (loc, ParseTree.Val name) ->
      (loc, Val name)
  | (loc, ParseTree.PatternMatching (t, patterns)) ->
      (loc, PatternMatching (unsugar_t t, List.map unsugar_pat patterns))
  | (loc, ParseTree.Let ((name, is_rec, (args, (annot, x))), t)) ->
      (loc, Let ((name, is_rec, unsugar_args args annot x), unsugar_t t))
  | (loc, ParseTree.Fail (ty, (exn, args))) ->
      (loc, Fail (unsugar_ty ty, (exn, List.map unsugar_t args)))
  | (loc, ParseTree.Try (t, patterns)) ->
      (loc, Try (unsugar_t t, List.map unsugar_try_pattern patterns))
  | (loc, ParseTree.Seq (x, y)) ->
      let name = Builtins.underscore in
      let ty = ((loc, Ty Builtins.t_unit), None) in
      (loc, Let ((name, NonRec, (Some ty, unsugar_t x)), unsugar_t y))

and unsugar_args args annot t =
  let rec aux = function
    | (loc, ParseTree.VArg (name, ty)) :: xs ->
        let ty = unsugar_ty ty in
        let (ty_xs, xs) = aux xs in
        let ty_xs =
          let aux (ty_xs, eff) =
            let ty_xs = Fun (ty, Option.default [] eff, ty_xs) in
            ((loc, ty_xs), None)
          in
          Option.map aux ty_xs
        in
        (ty_xs, (loc, Abs ((name, ty), xs)))
    | (loc, ParseTree.TArg (name, k)) :: xs ->
        let ty = (name, unsugar_kind k) in
        let (ty_xs, xs) = aux xs in
        let ty_xs =
          let aux (ty_xs, eff) =
            let ty_xs = Forall (ty, ty_xs) in
            ((loc, ty_xs), eff)
          in
          Option.map aux ty_xs
        in
        (ty_xs, (loc, TAbs (ty, xs)))
    | (loc, ParseTree.EArg name) :: xs ->
        let (ty_xs, xs) = aux xs in
        let ty_xs =
          let aux (ty_xs, eff) =
            let ty_xs = ForallEff (name, ty_xs) in
            ((loc, ty_xs), eff)
          in
          Option.map aux ty_xs
        in
        (ty_xs, (loc, EAbs (name, xs)))
    | (loc, ParseTree.Unit) :: xs ->
        let x =
          ParseTree.VArg
            (Builtins.underscore, (loc, ParseTree.Ty Builtins.t_unit))
        in
        aux ((loc, x) :: xs)
    | [] ->
        let aux (annot, eff) = (unsugar_ty annot, eff) in
        (Option.map aux annot, unsugar_t t)
  in
  aux args

let unsugar_variant (ParseTree.Variant (loc, name, ty)) =
  Variant (loc, name, unsugar_ty ty)

let unsugar_variants = List.map unsugar_variant

let create = function
  | (loc, ParseTree.Value (name, is_rec, (args, (ty, t)))) ->
      (loc, Value (name, is_rec, unsugar_args args ty t))
  | (loc, ParseTree.Type (name, ty)) ->
      (loc, Type (name, unsugar_ty ty))
  | (loc, ParseTree.Binding (name, ty, content)) ->
      (loc, Binding (name, unsugar_ty ty, content))
  | (loc, ParseTree.Datatype (name, k, variants)) ->
      (loc, Datatype (name, unsugar_kind k, unsugar_variants variants))
  | (loc, ParseTree.Exception (name, tys)) ->
      (loc, Exception (name, List.map unsugar_ty tys))

let create = List.map create

let create_interface = function
  | (loc, ParseTree.IVal (name, ty)) ->
      (loc, InterfaceTree.Val (name, unsugar_ty ty))
  | (loc, ParseTree.IAbstractType (name, k)) ->
      (loc, InterfaceTree.AbstractType (name, unsugar_kind k))
  | (loc, ParseTree.IDatatype (name, k, variants)) ->
      (loc, InterfaceTree.Datatype (name, unsugar_kind k, unsugar_variants variants))
  | (loc, ParseTree.ITypeAlias (name, ty)) ->
      (loc, InterfaceTree.TypeAlias (name, unsugar_ty ty))
  | (loc, ParseTree.IException (name, tys)) ->
      (loc, InterfaceTree.Exception (name, List.map unsugar_ty tys))

let create_interface = List.map create_interface
