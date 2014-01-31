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

let fmt = Printf.sprintf

type value = Gamma.value = {name : string; ty : Types.t}
type abs = {abs_ty : Types.t; param : value; ty_expr : Types.t}

type t =
  | Abs of (abs * t)
  | TAbs of (abs * t)
  | App of (Types.t * t * t)
  | TApp of (Types.t * t * Types.t)
  | Val of value

type variant =
  | Variant of (string * Types.t)

type top =
  | Value of (value * t)
  | Binding of (value * string)
  | Datatype of variant list

let get_type = function
  | Abs ({abs_ty; _}, _) -> abs_ty
  | TAbs ({abs_ty}, _) -> abs_ty
  | App (ty, _, _) -> ty
  | TApp (ty, _, _) -> ty
  | Val {ty; _} -> ty

let fail ~loc x = raise (Error.Exn (loc, x))
let type_error_msg =
  fmt
    "Error: This expression has type %s but an \
     expression was expected of type %s"

let type_error ~loc ~has ~expected =
  fail
    ~loc
    (type_error_msg
       (Types.to_string has)
       (Types.to_string expected)
    )

let function_type_error ~loc ~has ~expected =
  fail
    ~loc
    (fmt
       "Error: Can't apply %s to a non-function type %s"
       (Types.to_string has)
       (Types.to_string expected)
    )

let rec transform ~from ~ty =
  let replace = Types.replace ~from ~ty in
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
  in
  aux

let gamma_error = fmt "The value '%s' was not found in Γ"

let rec aux gamma gammaT = function
  | ParseTree.Abs (loc, (name, ty), t) ->
      let ty = Types.from_parse_tree ~loc gammaT ty in
      let param = {name; ty} in
      let expr = aux (param :: gamma) gammaT t in
      let ty_expr = get_type expr in
      let abs_ty = Types.Fun (ty, ty_expr) in
      Abs ({abs_ty; param; ty_expr}, expr)
  | ParseTree.TAbs (loc, name, t) ->
      let ty = Types.Ty name in
      let param = {name; ty} in
      let expr = aux gamma ((name, ty) :: gammaT) t in
      let ty_expr = get_type expr in
      let abs_ty = Types.Forall (name, ty_expr) in
      TAbs ({abs_ty; param; ty_expr}, expr)
  | ParseTree.App (loc, f, x) ->
      let f = aux gamma gammaT f in
      let x = aux gamma gammaT x in
      let ty_x = get_type x in
      begin match get_type f with
      | Types.Fun (ty, res) when Types.equal ty ty_x ->
          App (res, f, x)
      | Types.Fun (ty, _) -> type_error ~loc ~has:ty_x ~expected:ty
      | Types.Ty _ as ty -> function_type_error ~loc ~has:ty_x ~expected:ty
      | Types.Forall (ty, _) ->
          fail ~loc (type_error_msg (Types.to_string ty_x) ty)
      end
  | ParseTree.TApp (loc, f, ty_x) ->
      let f = aux gamma gammaT f in
      let ty_x = Types.from_parse_tree ~loc gammaT ty_x in
      begin match get_type f with
      | Types.Forall (ty, res) ->
          let res = Types.replace ~from:ty ~ty:ty_x res in
          let f = transform ~from:ty ~ty:ty_x f in
          TApp (res, f, ty_x)
      | Types.Fun (ty, _) -> type_error ~loc ~has:ty_x ~expected:ty
      | Types.Ty _ as ty -> function_type_error ~loc ~has:ty_x ~expected:ty
      end
  | ParseTree.Val (loc, name) ->
      let x = List.find (fun x -> String.equal name x.name) gamma in
      let x = Option.get_exn x (Error.Exn (loc, gamma_error name)) in
      Val x

let rec check_if_returns_type ~datatype = function
  | Types.Ty x -> String.equal x datatype
  | Types.Forall (_, ret)
  | Types.Fun (_, ret) -> check_if_returns_type ~datatype ret

let transform_variants ~datatype gammaT =
  let aux = function
    | ParseTree.Variant (loc, name, ty) ->
        let ty = Types.from_parse_tree ~loc gammaT ty in
        if check_if_returns_type ~datatype ty then
          Variant (name, ty), {name; ty}
        else
          fail ~loc "A variant doesn't return its type"
  in
  List.map aux

let rec from_parse_tree gamma gammaT = function
  | ParseTree.Value (loc, name, term) :: xs ->
      let x = aux gamma gammaT term in
      let v = {name; ty = get_type x} in
      let xs = from_parse_tree (v :: gamma) gammaT xs in
      Value (v, x) :: xs
  | ParseTree.Type (loc, name, ty) :: xs ->
      let ty = Types.from_parse_tree ~loc gammaT ty in
      from_parse_tree gamma ((name, ty) :: gammaT) xs
  | ParseTree.Binding (loc, name, ty, binding) :: xs ->
      let ty = Types.from_parse_tree ~loc gammaT ty in
      let v = {name; ty} in
      let xs = from_parse_tree (v :: gamma) gammaT xs in
      Binding (v, binding) :: xs
  | ParseTree.Datatype (loc, name, variants) :: xs ->
      let gammaT = (name, Types.Ty name) :: gammaT in
      let variants = transform_variants ~datatype:name gammaT variants in
      let xs = from_parse_tree (List.map snd variants @ gamma) gammaT xs in
      Datatype (List.map fst variants) :: xs
  | [] -> []
