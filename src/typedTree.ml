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

open MonadOpen

let fmt = Printf.sprintf

type value = Gamma.value = {name : string; ty : Types.t}
type abs = {abs_ty : Types.t; param : value; ty_expr : Types.t}

type t =
  | Abs of (abs * t)
  | TAbs of (abs * t)
  | App of (Types.t * t * t)
  | TApp of (Types.t * t * Types.t)
  | Val of value

type top =
  | Value of (value * t)
  | Binding of (value * string)

let get_type = function
  | Abs ({abs_ty; _}, _) -> abs_ty
  | TAbs ({abs_ty}, _) -> abs_ty
  | App (ty, _, _) -> ty
  | TApp (ty, _, _) -> ty
  | Val {ty; _} -> ty

let type_error_msg =
  fmt
    "Error: This expression has type %s but an \
     expression was expected of type %s"

let type_error ~has ~expected =
  failwith
    (type_error_msg
       (Types.to_string has)
       (Types.to_string expected)
    )

let function_type_error ~has ~expected =
  failwith
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

let rec aux gamma gammaT = function
  | ParseTree.Abs ((name, ty), t) ->
      Types.from_parse_tree gammaT ty >>= fun ty ->
      let param = {name; ty} in
      aux (param :: gamma) gammaT t >>= fun expr ->
      let ty_expr = get_type expr in
      let abs_ty = Types.Fun (ty, ty_expr) in
      Exn.return (Abs ({abs_ty; param; ty_expr}, expr))
  | ParseTree.TAbs (name, t) ->
      let ty = Types.Ty name in
      let param = {name; ty} in
      aux gamma ((name, ty) :: gammaT) t >>= fun expr ->
      let ty_expr = get_type expr in
      let abs_ty = Types.Forall (name, ty_expr) in
      Exn.return (TAbs ({abs_ty; param; ty_expr}, expr))
  | ParseTree.App (f, x) ->
      aux gamma gammaT f >>= fun f ->
      aux gamma gammaT x >>= fun x ->
      let ty_x = get_type x in
      begin match get_type f with
      | Types.Fun (ty, res) when Types.equal ty ty_x ->
          Exn.return (App (res, f, x))
      | Types.Fun (ty, _) -> type_error ~has:ty_x ~expected:ty
      | Types.Ty _ as ty -> function_type_error ~has:ty_x ~expected:ty
      | Types.Forall (ty, _) ->
          failwith (type_error_msg (Types.to_string ty_x) ty)
      end
  | ParseTree.TApp (f, ty_x) ->
      aux gamma gammaT f >>= fun f ->
      Types.from_parse_tree gammaT ty_x >>= fun ty_x ->
      begin match get_type f with
      | Types.Forall (ty, res) ->
          let res = Types.replace ~from:ty ~ty:ty_x res in
          let f = transform ~from:ty ~ty:ty_x f in
          Exn.return (TApp (res, f, ty_x))
      | Types.Fun (ty, _) -> type_error ~has:ty_x ~expected:ty
      | Types.Ty _ as ty -> function_type_error ~has:ty_x ~expected:ty
      end
  | ParseTree.Val name ->
      List.find (fun x -> Unsafe.(name = x.name)) gamma >>= fun x ->
      Exn.return (Val x)

let rec from_parse_tree gamma gammaT = function
  | ParseTree.Value (name, term) :: xs ->
      aux gamma gammaT term >>= fun x ->
      let v = {name; ty = get_type x} in
      from_parse_tree (v :: gamma) gammaT xs >>= fun xs ->
      Exn.return (Value (v, x) :: xs)
  | ParseTree.Type (name, ty) :: xs ->
      Types.from_parse_tree gammaT ty >>= fun ty ->
      from_parse_tree gamma ((name, ty) :: gammaT) xs
  | ParseTree.Binding (name, ty, binding) :: xs ->
      Types.from_parse_tree gammaT ty >>= fun ty ->
      let v = {name; ty} in
      from_parse_tree (v :: gamma) gammaT xs >>= fun xs ->
      Exn.return (Binding (v, binding) :: xs)
  | [] -> Exn.return []
