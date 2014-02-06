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

type t =
  | Ty of (string * Kinds.t)
  | Alias of (string * t)
  | Fun of (t * t)
  | Forall of (string * Kinds.t * t)
  | AbsOnTy of (string * Kinds.t * t)
  | AppOnTy of (t * t)

let fmt = Printf.sprintf

let type_error = fmt "The type '%s' was not found in Γ"

let rec from_parse_tree ~loc gammaT gammaK = function
  | ParseTree.Fun (x, y) ->
      let (x, _) = from_parse_tree ~loc gammaT gammaK x in
      let (y, _) = from_parse_tree ~loc gammaT gammaK y in
      (Fun (x, y), Kinds.Star)
  | ParseTree.Ty name ->
      begin match Gamma.Types.find name gammaT with
      | Some (ty, k) -> (Alias (name, ty), k)
      | None -> match Gamma.Kinds.find name gammaK with
        | Some k -> (Ty (name, k), k)
        | None -> raise (Error.Exn (loc, type_error name))
      end
  | ParseTree.Forall (name, k, ret) ->
      let (ret, _) = from_parse_tree ~loc gammaT (Gamma.Kinds.add name k gammaK) ret in
      (Forall (name, k, ret), Kinds.Star)
  | ParseTree.AbsOnTy (name, k, ret) ->
      let (ret, kret) = from_parse_tree ~loc gammaT (Gamma.Kinds.add name k gammaK) ret in
      (AbsOnTy (name, k, ret), Kinds.KFun (k, kret))
  | ParseTree.AppOnTy (f, x) ->
      let (f, kf) = from_parse_tree ~loc gammaT gammaK f in
      let (x, kx) = from_parse_tree ~loc gammaT gammaK x in
      let k =
        match kf with
        | Kinds.KFun (p, r) when Kinds.equal p kx -> r
        | (Kinds.KFun _ as k)
        | (Kinds.Star as k) ->
            raise
              (Error.Exn
                 (loc,
                  fmt "Kind '%s' can't be applied on '%s'"
                    (Kinds.to_string kx)
                    (Kinds.to_string k)
                 )
              )
      in
      (AppOnTy (f, x), k)
