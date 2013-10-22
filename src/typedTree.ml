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

type value = {name : string; ty : Types.t}
type abs = {abs_ty : Types.t; param : value; ty_expr : Types.t}

type t =
  | Abs of (abs * t)
  | App of (Types.t * t * t)
  | Val of value

type top =
  | Value of (value * t)

let get_type = function
  | Abs ({abs_ty; _}, _) -> abs_ty
  | App (ty, _, _) -> ty
  | Val {ty; _} -> ty

let rec aux gamma gammaT = function
  | ParseTree.Abs ((name, ty), t) ->
      Types.from_parse_tree gammaT ty >>= fun ty ->
      let param = {name; ty} in
      aux (param :: gamma) gammaT t >>= fun expr ->
      let ty_expr = get_type expr in
      let abs_ty = Types.Fun (ty, ty_expr) in
      Exn.return (Abs ({abs_ty; param; ty_expr}, expr))
  | ParseTree.App (f, x) ->
      aux gamma gammaT f >>= fun f ->
      aux gamma gammaT x >>= fun x ->
      let ty_x = get_type x in
      begin match get_type f with
      | Types.Fun (ty, res) when Types.equal ty ty_x ->
          Exn.return (App (res, f, x))
      | Types.Fun (ty, _) ->
          failwith
            ("Error: This expression has type "
             ^ Types.to_string ty_x
             ^ " but an expression was expected of type "
             ^ Types.to_string ty
            )
      | Types.Ty _ as ty ->
          failwith
            ("Typechecker: Can't apply to a non-function type ("
             ^ Types.to_string ty
             ^ " to "
             ^ Types.to_string ty_x
             ^ ")"
            )
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
  | [] -> Exn.return []
