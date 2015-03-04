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

(* NOTE: Types are in normal form *)

open BatteriesExceptionless
open Monomorphic.None

type name = Ident.Type.t

type t =
  | Ty of name
  | Fun of (t * Effects.t * t)
  | Forall of (name * Kinds.t * t)
  | AbsOnTy of (name * Kinds.t * t)
  | AppOnTy of (t * t)

type visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

let fmt = Printf.sprintf

let fail_not_star ~loc x =
  Error.fail ~loc "The type construct '%s' cannot be applied with kind /= '*'" x

let fail_apply ~loc x f =
  let conv = Kinds.to_string in
  Error.fail ~loc "Kind '%s' can't be applied on '%s'" (conv x) (conv f)

let kind_missmatch ~loc ~has ~on =
  Error.fail
    ~loc
    "Cannot apply something with kind '%s' on '%s'"
    (Kinds.to_string has)
    (Kinds.to_string on)

let rec replace ~from ~ty =
  let rec aux = function
    | Ty x when Ident.Type.equal x from -> ty
    | Ty _ as t -> t
    | Fun (param, eff, ret) -> Fun (aux param, eff, aux ret)
    | (AbsOnTy (x, _, _) as t)
    | (Forall (x, _, _) as t) when Ident.Type.equal x from -> t
    | Forall (x, k, t) -> Forall (x, k, aux t)
    | (AbsOnTy (x, _, _) as t) when Ident.Type.equal x from -> t
    | AbsOnTy (x, k, t) -> AbsOnTy (x, k, aux t)
    | AppOnTy (f, x) ->
        let x = aux x in
        begin match aux f with
        | AbsOnTy (from', _, t) -> replace ~from:from' ~ty:x t
        | (Ty _ as f)
        | (AppOnTy _ as f) -> AppOnTy (f, x)
        | Fun _
        | Forall _ -> assert false
        end
  in
  aux

let rec of_parse_tree_kind gammaT = function
  | (loc, ParseTree.Fun (x, eff, y)) ->
      let (x, k1) = of_parse_tree_kind gammaT x in
      let eff =
        let aux acc ty = Effects.add ~loc ty acc in
        List.fold_left aux Effects.empty eff
      in
      let (y, k2) = of_parse_tree_kind gammaT y in
      if Kinds.not_star k1 || Kinds.not_star k2 then
        fail_not_star ~loc "->";
      (Fun (x, eff, y), Kinds.Star)
  | (loc, ParseTree.Ty name) ->
      begin match GammaMap.Types.find name gammaT with
      | Some (Alias (ty, k)) -> (ty, k)
      | Some (Abstract k) -> (Ty name, k)
      | None ->
          Error.fail
            ~loc
            "The type '%s' was not found in Γ"
            (Ident.Type.to_string name)
      end
  | (loc, ParseTree.Forall ((name, k), ret)) ->
      let gammaT = GammaMap.Types.add ~loc name (Abstract k) gammaT in
      let (ret, kx) = of_parse_tree_kind gammaT ret in
      if Kinds.not_star kx then
        fail_not_star ~loc "forall";
      (Forall (name, k, ret), Kinds.Star)
  | (loc, ParseTree.AbsOnTy ((name, k), ret)) ->
      let gammaT = GammaMap.Types.add ~loc name (Abstract k) gammaT in
      let (ret, kret) = of_parse_tree_kind gammaT ret in
      (AbsOnTy (name, k, ret), Kinds.KFun (k, kret))
  | (loc, ParseTree.AppOnTy ((_, ParseTree.AbsOnTy ((name, k), t)), x)) ->
      let (x, kx) = of_parse_tree_kind gammaT x in
      if not (Kinds.equal k kx) then
        kind_missmatch ~loc ~has:kx ~on:k;
      let gammaT = GammaMap.Types.add ~loc name (Abstract k) gammaT in
      let (t, kt) = of_parse_tree_kind gammaT t in
      (replace ~from:name ~ty:x t, kt)
  | (loc, ParseTree.AppOnTy (f, x)) ->
      let (f, kf) = of_parse_tree_kind gammaT f in
      let (x, kx) = of_parse_tree_kind gammaT x in
      let k =
        match kf with
        | Kinds.KFun (p, r) when Kinds.equal p kx -> r
        | (Kinds.KFun _ as k)
        | (Kinds.Star as k) -> fail_apply ~loc kx k
      in
      (AppOnTy (f, x), k)

let of_parse_tree gammaT ty =
  let (loc, _) = ty in
  let (ty, k) = of_parse_tree_kind gammaT ty in
  if Kinds.not_star k then
    Error.fail ~loc "Values cannot be of kind /= '*'";
  ty

let func ~param ~eff ~res = Fun (param, eff, res)
let forall ~param ~kind ~res = Forall (param, kind, res)

let rec to_string = function
  | Ty x -> Ident.Type.to_string x
  | Fun (Ty x, eff, ret) ->
      fmt "%s %s %s" (Ident.Type.to_string x) (Effects.to_string eff) (to_string ret)
  | Fun (x, eff, ret) ->
      fmt "(%s) %s %s" (to_string x) (Effects.to_string eff) (to_string ret)
  | Forall (x, k, t) ->
      fmt "forall %s : %s. %s" (Ident.Type.to_string x) (Kinds.to_string k) (to_string t)
  | AbsOnTy (name, k, t) ->
      fmt "λ%s : %s. %s" (Ident.Type.to_string name) (Kinds.to_string k) (to_string t)
  | AppOnTy (Ty f, Ty x) -> fmt "%s %s" (Ident.Type.to_string f) (Ident.Type.to_string x)
  | AppOnTy (Ty f, x) -> fmt "%s (%s)" (Ident.Type.to_string f) (to_string x)
  | AppOnTy (f, Ty x) -> fmt "(%s) %s" (to_string f) (Ident.Type.to_string x)
  | AppOnTy (f, x) -> fmt "(%s) (%s)" (to_string f) (to_string x)

let equal eff_eq x y =
  let rec aux eq_list = function
    | Ty x, Ty x' ->
        let eq = Ident.Type.equal in
        List.exists (fun (y, y') -> eq x y && eq x' y') eq_list
        || (eq x x' && List.for_all (fun (y, y') -> eq x y || eq x' y') eq_list)
    | Fun (param, eff1, res), Fun (param', eff2, res') ->
        aux eq_list (param, param')
        && eff_eq eff1 eff2
        && aux eq_list (res, res')
    | AppOnTy (f, x), AppOnTy (f', x') ->
        aux eq_list (f, f') && aux eq_list (x, x')
    | AbsOnTy (name1, k1, t), AbsOnTy (name2, k2, t')
    | Forall (name1, k1, t), Forall (name2, k2, t') when Kinds.equal k1 k2 ->
        aux ((name1, name2) :: eq_list) (t, t')
    | AppOnTy _, _
    | AbsOnTy _, _
    | Forall _, _
    | Ty _, _
    | Fun _, _ -> false
  in
  aux [] (x, y)

let is_subset_of = equal Effects.is_subset_of

let equal = equal Effects.equal

let rec size = function
  | Fun (_, _, t) -> succ (size t)
  | AppOnTy _
  | AbsOnTy _
  | Ty _ -> 0
  | Forall (_, _, t) -> size t

let rec head = function
  | Ty name -> name
  | Fun (_, _, t)
  | Forall (_, _, t)
  | AbsOnTy (_, _, t)
  | AppOnTy (t, _) -> head t

module Error = struct
  let type_error_aux ~loc =
    Error.fail
      ~loc
      "Error: This expression has type '%s' but an \
       expression was expected of type '%s'"

  let fail ~loc ~has ~expected =
    type_error_aux ~loc (to_string has) (to_string expected)

  let function_type ~loc ty =
    Error.fail
      ~loc
      "Error: This expression has type '%s'. \
       This is not a function; it cannot be applied."
      (to_string ty)

  let forall_type ~loc ty =
    Error.fail
      ~loc
      "Error: This expression has type '%s'. \
       This is not a type abstraction; it cannot be applied by a value."
      (to_string ty)

  let fail_return_type ~loc name =
    Error.fail
      ~loc
      "The variant '%s' doesn't return its type"
      (Ident.Name.to_string name)
end

let apply ~loc = function
  | Fun x ->
      x
  | (Forall _ as ty)
  | (AppOnTy _ as ty)
  | (Ty _ as ty) ->
      Error.function_type ~loc ty
  | AbsOnTy _ ->
      assert false

let apply_ty ~loc ~ty_x ~kind_x = function
  | Forall (ty, k, res) when Kinds.equal k kind_x ->
      let res = replace ~from:ty ~ty:ty_x res in
      (ty, res)
  | Forall (_, k, _) ->
      kind_missmatch ~loc ~has:kind_x ~on:k
  | (Fun _ as ty)
  | (AppOnTy _ as ty)
  | (Ty _ as ty) ->
      Error.forall_type ~loc ty
  | AbsOnTy _ ->
      assert false

let rec check_if_returns_type ~datatype = function
  | Ty x -> Ident.Type.equal x datatype
  | Forall (_, _, ret)
  | AppOnTy (ret, _)
  | Fun (_, _, ret) -> check_if_returns_type ~datatype ret
  | AbsOnTy _ -> false

let rec has_io = function
  | Fun (_, eff, Forall _)
  | Fun (_, eff, Ty _) -> Effects.has_io eff
  | Forall (_, _, ret)
  | AppOnTy (ret, _)
  | Fun (_, _, ret) -> has_io ret
  | Ty _
  | AbsOnTy _ -> true
