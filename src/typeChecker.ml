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

open TypedTree

let get_type = function
  | Abs ({abs_ty; _}, _) -> abs_ty
  | TAbs ({abs_ty; _}, _) -> abs_ty
  | App (ty, _, _) -> ty
  | TApp (ty, _, _) -> ty
  | Val {ty; _} -> ty
  | PatternMatching (_, _, ty) -> ty

let type_error_aux ~loc =
  Error.fail
    ~loc
    "Error: This expression has type '%s' but an \
     expression was expected of type '%s'"

let type_error ~loc ~has ~expected =
  type_error_aux ~loc (TypesBeta.to_string has) (TypesBeta.to_string expected)

let function_type_error ~loc ~has ~expected =
  Error.fail
    ~loc
    "Error: Can't apply '%s' to a non-function type '%s'"
    (TypesBeta.to_string has)
    (TypesBeta.to_string expected)

let kind_missmatch ~loc ~has ~on =
  Error.fail
    ~loc
    "Cannot apply something with kind '%s' on '%s'"
    (Kinds.to_string has)
    (Kinds.to_string on)

let rec transform ~from ~ty =
  let replace = TypesBeta.replace ~from ~ty in
  let transform x = transform ~from ~ty x in
  let rec aux = function
    | Abs ({abs_ty; param = {name; ty}; ty_expr}, t) ->
        let abs_ty = replace abs_ty in
        let ty = replace ty in
        let ty_expr = replace ty_expr in
        Abs ({abs_ty; param = {name; ty}; ty_expr}, aux t)
    | TAbs ({abs_ty; param = {name; ty}; ty_expr}, t) ->
        let abs_ty = replace abs_ty in
        let ty = replace ty in
        let ty_expr = replace ty_expr in
        let t = transform t in
        TAbs ({abs_ty; param = {name; ty}; ty_expr}, aux t)
    | App (ty, f, x) ->
        let ty = replace ty in
        App (ty, aux f, aux x)
    | TApp (ty, f, x) ->
        let ty = replace ty in
        let f = transform f in
        let x = replace x in
        TApp (ty, aux f, x)
    | Val {name; ty} ->
        let ty = replace ty in
        Val {name; ty}
    | PatternMatching (t, patterns, ty) ->
        let t = transform t in
        let patterns = Pattern.Matrix.map aux patterns in
        let ty = replace ty in
        PatternMatching (t, patterns, ty)
  in
  aux

let ty_from_parse_tree ~loc gammaT gammaK ty =
  let (ty, k) = Types.from_parse_tree ~loc gammaT gammaK ty in
  (TypesBeta.of_ty ty, k)

let ty_from_parse_tree' ~loc gammaT gammaK ty =
  let (ty, k) = Types.from_parse_tree ~loc gammaT gammaK ty in
  if Kinds.not_star k then
    Error.fail ~loc "Values cannot be of kind /= '*'";
  TypesBeta.of_ty ty

let rec aux gamma gammaT gammaK gammaC = function
  | ParseTree.Abs (loc, (name, ty), t) ->
      let ty = ty_from_parse_tree' ~loc gammaT gammaK ty in
      let expr = aux (Gamma.Value.add name ty gamma) gammaT gammaK gammaC t in
      let param = {name; ty} in
      let ty_expr = get_type expr in
      let abs_ty = TypesBeta.Fun (ty, ty_expr) in
      Abs ({abs_ty; param; ty_expr}, expr)
  | ParseTree.TAbs (loc, (name, k), t) ->
      let ty = TypesBeta.Ty name in
      let param = {name; ty = ty} in
      let expr = aux gamma gammaT (Gamma.Kinds.add ~loc name k gammaK) gammaC t in
      let ty_expr = get_type expr in
      let abs_ty = TypesBeta.Forall (name, k, ty_expr) in
      TAbs ({abs_ty; param; ty_expr}, expr)
  | ParseTree.App (loc, f, x) ->
      let f = aux gamma gammaT gammaK gammaC f in
      let x = aux gamma gammaT gammaK gammaC x in
      let ty_x = get_type x in
      begin match get_type f with
      | TypesBeta.Fun (ty, res) when TypesBeta.equal ty ty_x ->
          App (res, f, x)
      | TypesBeta.Fun (ty, _) -> type_error ~loc ~has:ty_x ~expected:ty
      | (TypesBeta.AppOnTy _ as ty)
      | (TypesBeta.Ty _ as ty) -> function_type_error ~loc ~has:ty_x ~expected:ty
      | TypesBeta.Forall (ty, _, _) ->
          type_error_aux ~loc (TypesBeta.to_string ty_x) ty
      | TypesBeta.AbsOnTy _ -> assert false
      end
  | ParseTree.TApp (loc, f, ty_x) ->
      let f = aux gamma gammaT gammaK gammaC f in
      let (ty_x, kx) = ty_from_parse_tree ~loc gammaT gammaK ty_x in
      begin match get_type f with
      | TypesBeta.Forall (ty, k, res) when Kinds.equal k kx ->
          let res = TypesBeta.replace ~from:ty ~ty:ty_x res in
          let f = transform ~from:ty ~ty:ty_x f in
          TApp (res, f, ty_x)
      | TypesBeta.Forall (_, k, _) -> kind_missmatch ~loc ~has:kx ~on:k
      | TypesBeta.Fun (ty, _) -> type_error ~loc ~has:ty_x ~expected:ty
      | (TypesBeta.AppOnTy _ as ty)
      | (TypesBeta.Ty _ as ty) -> function_type_error ~loc ~has:ty_x ~expected:ty
      | TypesBeta.AbsOnTy _ -> assert false
      end
  | ParseTree.Val (loc, name) ->
      begin match Gamma.Value.find name gamma with
      | None -> Error.fail ~loc "The value '%s' was not found in Γ" name
      | Some ty -> Val {name; ty}
      end
  | ParseTree.PatternMatching (loc, t, patterns) ->
      let t = aux gamma gammaT gammaK gammaC t in
      let ty = get_type t in
      let (head, tail) = match patterns with
        | [] -> assert false
        | x::xs -> (x, xs)
      in
      let (initial_pattern, initial_ty) =
        let term = aux gamma gammaT gammaK gammaC (snd head) in
        (Pattern.Matrix.create ~loc gammaC ty term (fst head), get_type term)
      in
      let patterns =
        let f patterns (p, t) =
          let t = aux gamma gammaT gammaK gammaC t in
          let has = get_type t in
          if not (TypesBeta.equal has initial_ty) then
            type_error ~loc ~has ~expected:initial_ty;
          Pattern.Matrix.append ~loc gammaC ty t p patterns
        in
        List.fold_left f initial_pattern tail
      in
      PatternMatching (t, patterns, initial_ty)

let rec check_if_returns_type ~datatype = function
  | TypesBeta.Ty x -> String.equal x datatype
  | TypesBeta.Forall (_, _, ret)
  | TypesBeta.AppOnTy (ret, _)
  | TypesBeta.Fun (_, ret) -> check_if_returns_type ~datatype ret
  | TypesBeta.AbsOnTy _ -> false

let transform_variants ~datatype gamma gammaT gammaK gammaC =
  let rec aux = function
    | ParseTree.Variant (loc, name, ty) :: xs ->
        let ty = ty_from_parse_tree' ~loc gammaT gammaK ty in
        if check_if_returns_type ~datatype ty then
          let (xs, gamma, gammaC) = aux xs in
          let gamma = Gamma.Value.add name ty gamma in
          let gammaC = Gamma.Constr.add name ty gammaC in
          (Variant (name, ty) :: xs, gamma, gammaC)
        else
          Error.fail ~loc "The variant '%s' doesn't return its type" name
    | [] -> ([], gamma, gammaC)
  in
  aux

let rec from_parse_tree gamma gammaT gammaK gammaC = function
  | ParseTree.Value (name, term) :: xs ->
      let x = aux gamma gammaT gammaK gammaC term in
      let ty = get_type x in
      let xs = from_parse_tree (Gamma.Value.add name ty gamma) gammaT gammaK gammaC xs in
      Value ({name; ty}, x) :: xs
  | ParseTree.Type (loc, name, ty) :: xs ->
      let ty = Types.from_parse_tree ~loc gammaT gammaK ty in
      from_parse_tree gamma (Gamma.Types.add ~loc name ty gammaT) gammaK gammaC xs
  | ParseTree.Binding (loc, name, ty, binding) :: xs ->
      let ty = ty_from_parse_tree' ~loc gammaT gammaK ty in
      let xs = from_parse_tree (Gamma.Value.add name ty gamma) gammaT gammaK gammaC xs in
      Binding ({name; ty}, binding) :: xs
  | ParseTree.Datatype (loc, name, kind, variants) :: xs ->
      let gammaK = Gamma.Kinds.add ~loc name kind gammaK in
      let (variants, gamma, gammaC) = transform_variants ~datatype:name gamma gammaT gammaK gammaC variants in
      let xs = from_parse_tree gamma gammaT gammaK gammaC xs in
      Datatype variants :: xs
  | [] -> []
