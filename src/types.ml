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

type name = Gamma.Key.t

type t =
  | Ty of (name * Kinds.t)
  | Alias of (name * t)
  | Fun of (t * t)
  | Forall of (name * Kinds.t * t)
  | AbsOnTy of (name * Kinds.t * t)
  | AppOnTy of (t * t)

let fail_not_star ~loc x =
  Error.fail ~loc "The type construct '%s' cannot be applied with kind /= '*'" x

let fail_apply ~loc x f =
  let conv = Kinds.to_string in
  Error.fail ~loc "Kind '%s' can't be applied on '%s'" (conv x) (conv f)

let rec from_parse_tree ~loc gammaT = function
  | ParseTree.Fun (x, y) ->
      let (x, k1) = from_parse_tree ~loc gammaT x in
      let (y, k2) = from_parse_tree ~loc gammaT y in
      if Kinds.not_star k1 || Kinds.not_star k2 then
        fail_not_star ~loc "->";
      (Fun (x, y), Kinds.Star)
  | ParseTree.Ty name ->
      begin match Gamma.Types.find name gammaT with
      | Some (`Alias (ty, k)) -> (Alias (name, ty), k)
      | Some (`Abstract k) -> (Ty (name, k), k)
      | None -> Error.fail ~loc "The type '%s' was not found in Γ" (Gamma.Key.to_string name)
      end
  | ParseTree.Forall (name, k, ret) ->
      let (ret, kx) = from_parse_tree ~loc (Gamma.Types.add ~loc name (`Abstract k) gammaT) ret in
      if Kinds.not_star kx then
        fail_not_star ~loc "forall";
      (Forall (name, k, ret), Kinds.Star)
  | ParseTree.AbsOnTy (name, k, ret) ->
      let (ret, kret) = from_parse_tree ~loc (Gamma.Types.add ~loc name (`Abstract k) gammaT) ret in
      (AbsOnTy (name, k, ret), Kinds.KFun (k, kret))
  | ParseTree.AppOnTy (f, x) ->
      let (f, kf) = from_parse_tree ~loc gammaT f in
      let (x, kx) = from_parse_tree ~loc gammaT x in
      let k =
        match kf with
        | Kinds.KFun (p, r) when Kinds.equal p kx -> r
        | (Kinds.KFun _ as k)
        | (Kinds.Star as k) -> fail_apply ~loc kx k
      in
      (AppOnTy (f, x), k)
