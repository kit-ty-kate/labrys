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

type value = (string * Types.t)

type t =
  | Abs of ((Types.t * value * Types.t) * t)
  | App of (Types.t * t * t)
  | Val of value

type top =
  | Value of (value * t)

let get_type = function
  | Abs ((_, _, ty), _) -> ty
  | App (ty, _, _) -> ty
  | Val (_, ty) -> ty

let rec aux gamma gammaT = function
  | ParseTree.Abs ((name, ty), t) ->
      Types.from_parse_tree gammaT ty >>= fun ty ->
      let v = (name, ty) in
      aux (v :: gamma) gammaT t >>= fun x ->
      let ty_x = get_type x in
      Exn.return (Abs ((ty_x, v, Ast.Fun (ty, ty_x)), x))
  | ParseTree.App (f, x) ->
      aux gamma gammaT f >>= fun f ->
      aux gamma gammaT x >>= fun x ->
      let ty_x = get_type x in
      (match get_type f with
        | Ast.Fun (ty, res) when Unsafe.(ty = ty_x) ->
            Exn.return (App (res, f, x))
        | Ast.Fun (ty, _) ->
            failwith
              ("Error: This expression has type "
               ^ Types.to_string ty_x
               ^ " but an expression was expected of type "
               ^ Types.to_string ty
              )
        | Ast.Ty _ as ty ->
            failwith
              ("Typechecker: Can't apply to a non-function type ("
               ^ Types.to_string ty
               ^ " to "
               ^ Types.to_string ty_x
               ^ ")"
              )
      )
  | ParseTree.Val name ->
      List.find (fun (name', _) -> Unsafe.(name = name')) gamma >>= fun x ->
      Exn.return (Val x)

let rec from_parse_tree gamma gammaT = function
  | ParseTree.Value (name, term) :: xs ->
      aux gamma gammaT term >>= fun x ->
      let v = (name, get_type x) in
      from_parse_tree (v :: gamma) gammaT xs >>= fun xs ->
      Exn.return (Value (v, x) :: xs)
  | [] -> Exn.return []
