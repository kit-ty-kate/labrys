(*
Copyright (c) 2013-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

open Monomorphic_containers.Open

type name = Ident.Type.t

type tyclass_arg = PrivateTypes.tyclass_arg =
  | Param of (name * Kinds.t)
  | Filled of t

and t = PrivateTypes.t =
  | Ty of name
  | Eff of PrivateTypes.effects
  | Fun of (t * PrivateTypes.effects * t)
  | Forall of (name * Kinds.t * t)
  | TyClass of ((Ident.TyClass.t * tyclass_arg list) * PrivateTypes.effects * t)
  | AbsOnTy of (name * Kinds.t * t)
  | AppOnTy of (t * t)

type visibility = PrivateTypes.visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

let fmt = Printf.sprintf

let empty options =
  let aux gammaT eff = GammaMap.Types.add eff (Abstract Kinds.Eff) gammaT in
  List.fold_left aux GammaMap.Types.empty (Builtins.effects options)

let fail_not_star ~loc x =
  Err.fail ~loc "The type construct '%s' cannot be used with kind /= '*'" x

let kind_missmatch ~loc_x ~has ~expected =
  Err.fail
    ~loc:loc_x
    "Error: This type has kind '%s' but a \
     type was expected of kind '%s'"
    (Kinds.to_string has)
    (Kinds.to_string expected)

let rec replace ~from ~ty =
  let rec aux = function
    | Ty x when Ident.Type.equal x from -> ty
    | Ty _ as t -> t
    | Eff effects -> Eff (Effects.replace ~from ~ty effects)
    | Fun (param, eff, ret) ->
        Fun (aux param, Effects.replace ~from ~ty eff, aux ret)
    | Forall (x, k, t) -> Forall (x, k, aux t)
    | TyClass ((x, args), eff, t) ->
        let args =
          let aux = function
            | Param _ as arg -> arg
            | Filled ty -> Filled (aux ty)
          in
          List.map aux args
        in
        let eff = Effects.replace ~from ~ty eff in
        TyClass ((x, args), eff, aux t)
    | AbsOnTy (x, k, t) -> AbsOnTy (x, k, aux t)
    | AppOnTy (f, x) ->
        let x = aux x in
        begin match aux f with
        | AbsOnTy (from', _, t) -> replace ~from:from' ~ty:x t
        | (Ty _ as f)
        | (AppOnTy _ as f) -> AppOnTy (f, x)
        | Eff _ | Fun _ | Forall _ | TyClass _ -> assert false
        end
  in
  aux

let switch_pure_arrow (_, pure_arrow) = match pure_arrow with
  | `Allow -> (true, pure_arrow)
  | `Partial | `Forbid -> (false, pure_arrow)

let handle_effects ~loc options (b, _) gammaExn gammaT = function
  | Some eff -> Effects.of_list options gammaExn gammaT eff
  | None when b -> Effects.empty
  | None ->
      (* TODO: Be more precise *)
      Err.fail
        ~loc
        "Pure arrows are forbidden here. If you really want one use the \
         explicit syntax '-[]->' instead"

(* TODO: Check if it's really in normal form all the time *)
let rec of_parse_tree_kind ~pure_arrow options gammaT gammaExn gammaTC = function
  | (loc, UnsugaredTree.Fun (x, eff, y)) ->
      let loc_x = fst x in
      let loc_y = fst y in
      let pa = switch_pure_arrow pure_arrow in
      let (x, k1) = of_parse_tree_kind ~pure_arrow:pa options gammaT gammaExn gammaTC x in
      let eff = handle_effects ~loc options pure_arrow gammaExn gammaT eff in
      let (y, k2) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn gammaTC y in
      if Kinds.not_star k1 then
        fail_not_star ~loc:loc_x "->";
      if Kinds.not_star k2 then
        fail_not_star ~loc:loc_y "->";
      (Fun (x, eff, y), Kinds.Star)
  | (_, UnsugaredTree.Ty name) ->
      begin match GammaMap.Types.fill_module name gammaT with
      | (_, Alias (ty, k)) -> (ty, k) (* TODO: Fix variables if already exist *)
      | (name, Abstract k) -> (Ty name, k)
      end
  | (_, UnsugaredTree.Eff effects) ->
      (Eff (Effects.of_list options gammaExn gammaT effects), Kinds.Eff)
  | (_, UnsugaredTree.Forall ((name, k), ret)) ->
      let loc_ret = fst ret in
      let gammaT = GammaMap.Types.add name (Abstract k) gammaT in
      let (ret, kx) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn gammaTC ret in
      if Kinds.not_star kx then
        fail_not_star ~loc:loc_ret "forall";
      (Forall (name, k, ret), Kinds.Star)
  | (loc, UnsugaredTree.TyClass ((name, args), eff, ret)) -> (* TODO: Handle parameters that appears in several classes *)
      let loc_ret = fst ret in
      let (name, args) =
        let (name, tyclass) = GammaMap.TyClass.fill_module name gammaTC in
        let loc = Ident.TyClass.loc name in
        let args = Class.get_params ~loc (List.length args) tyclass in
        (name, args)
      in
      let gammaT =
        let aux gammaT = function
          | Param (x, k) -> GammaMap.Types.add x (Abstract k) gammaT
          | Filled _ -> gammaT
        in
        List.fold_left aux gammaT args
      in
      let eff = handle_effects ~loc options pure_arrow gammaExn gammaT eff in
      let (ret, kx) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn gammaTC ret in
      if Kinds.not_star kx then
        fail_not_star ~loc:loc_ret "forall";
      (TyClass ((name, args), eff, ret), Kinds.Star)
  | (_, UnsugaredTree.AbsOnTy ((name, k), ret)) ->
      let gammaT = GammaMap.Types.add name (Abstract k) gammaT in
      let (ret, kret) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn gammaTC ret in
      (AbsOnTy (name, k, ret), Kinds.KFun (k, kret))
  | (_, UnsugaredTree.AppOnTy ((_, UnsugaredTree.AbsOnTy ((name, k), t)), x)) ->
      let loc_x = fst x in
      let pa = switch_pure_arrow pure_arrow in
      let (x, kx) = of_parse_tree_kind ~pure_arrow:pa options gammaT gammaExn gammaTC x in
      if not (Kinds.equal k kx) then
        kind_missmatch ~loc_x ~has:kx ~expected:k;
      let gammaT = GammaMap.Types.add name (Abstract k) gammaT in
      let (t, kt) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn gammaTC t in
      (replace ~from:name ~ty:x t, kt)
  | (loc, UnsugaredTree.AppOnTy (f, x)) ->
      let (f, kf) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn gammaTC f in
      let pa = switch_pure_arrow pure_arrow in
      let (x, kx) = of_parse_tree_kind ~pure_arrow:pa options gammaT gammaExn gammaTC x in
      let k =
        match kf with
        | Kinds.KFun (p, r) when Kinds.equal p kx -> r
        | (Kinds.KFun _ as k)
        | (Kinds.Eff as k)
        | (Kinds.Star as k) ->
            Err.fail
              ~loc
              "Kind '%s' can't be applied on '%s'"
              (Kinds.to_string kx)
              (Kinds.to_string k)
      in
      (AppOnTy (f, x), k)

let of_parse_tree_kind ~pure_arrow =
  let pure_arrow = match pure_arrow with
    | `Allow -> (true, pure_arrow)
    | `Partial -> (true, pure_arrow)
    | `Forbid -> (false, pure_arrow)
  in
  of_parse_tree_kind ~pure_arrow

let of_parse_tree ~pure_arrow options gammaT gammaExn gammaTC ty =
  let (loc, _) = ty in
  let (ty, k) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn gammaTC ty in
  if Kinds.not_star k then
    Err.fail ~loc "Values cannot be of kind /= '*'";
  ty

let rec to_string =
  let tyclass_arg_to_string = function
    | Param (x, _) -> Ident.Type.to_string x
    | Filled ty -> fmt "[%s]" (to_string ty)
  in
  let tyclass_args_to_string args =
    String.concat " " (List.map tyclass_arg_to_string args)
  in
  function
  | Ty x -> Ident.Type.to_string x
  | Eff effects -> Effects.to_string effects
  | Fun (Ty x, eff, ret) when Effects.is_empty eff ->
      fmt "%s -> %s" (Ident.Type.to_string x) (to_string ret)
  | Fun (Ty x, eff, ret) ->
      fmt "%s -%s-> %s" (Ident.Type.to_string x) (Effects.to_string eff) (to_string ret)
  | Fun (x, eff, ret) ->
      fmt "(%s) %s %s" (to_string x) (Effects.to_string eff) (to_string ret)
  | Forall (x, k, t) ->
      fmt "forall %s : %s. %s" (Ident.Type.to_string x) (Kinds.to_string k) (to_string t)
  | TyClass ((name, args), eff, t) when Effects.is_empty eff ->
      fmt "{%s %s} => %s" (Ident.TyClass.to_string name) (tyclass_args_to_string args) (to_string t)
  | TyClass ((name, args), eff, t) ->
      fmt "{%s %s} =%s=> %s" (Ident.TyClass.to_string name) (tyclass_args_to_string args) (Effects.to_string eff) (to_string t)
  | AbsOnTy (name, k, t) ->
      fmt "λ%s : %s. %s" (Ident.Type.to_string name) (Kinds.to_string k) (to_string t)
  | AppOnTy (Ty f, Ty x) -> fmt "%s %s" (Ident.Type.to_string f) (Ident.Type.to_string x)
  | AppOnTy (Ty f, x) -> fmt "%s (%s)" (Ident.Type.to_string f) (to_string x)
  | AppOnTy (f, Ty x) -> fmt "(%s) %s" (to_string f) (Ident.Type.to_string x)
  | AppOnTy (f, x) -> fmt "(%s) (%s)" (to_string f) (to_string x)

let equal = PrivateTypes.ty_equal
let is_subset_of = PrivateTypes.ty_is_subset_of

let rec size = function
  | Fun (_, _, t) | TyClass (_, _, t) -> succ (size t)
  | AppOnTy _ | AbsOnTy _ | Eff _ | Ty _ -> 0
  | Forall (_, _, t) -> size t

let rec is_value = function
  | Ty _ -> true
  | Eff _ | AbsOnTy _ -> assert false
  | Fun _ | Forall _ | TyClass _ -> false
  | AppOnTy (t, _) -> is_value t

let rec head = function
  | Ty name -> (name, [])
  | Eff _
  | Fun _
  | Forall _
  | TyClass _
  | AbsOnTy _ -> assert false
  | AppOnTy (t, y) ->
      let (x, xs) = head t in
      (x, y :: xs)

let remove_module_aliases = PrivateTypes.ty_remove_module_aliases

module Err = struct
  let fail ~loc_t ~has ~expected =
    Err.fail
      ~loc:loc_t
      "Error: This expression has type '%s' but an \
       expression was expected of type '%s'"
      (to_string has)
      (to_string expected)

  let function_type ~loc_f ty =
    Err.fail
      ~loc:loc_f
      "Error: This expression has type '%s'. \
       This is not a function; it cannot be applied."
      (to_string ty)

  let forall_type ~loc_f ty =
    Err.fail
      ~loc:loc_f
      "Error: This expression has type '%s'. \
       This is not a type abstraction; it cannot be applied by a value."
      (to_string ty)
end

(* TODO: Improve and finish *)
let rec match_tyclass ~loc_x ~tyclasses ~ty_x =
  let eq name =
    let aux = function
      | Param (x, _) -> Ident.Type.equal name x
      | Filled _ -> false
    in
    let aux ((_, args), _) = List.exists aux args in
    List.exists aux tyclasses
  in
  function
  | Ty name when eq name ->
      [(name, ty_x)]
  | x when is_subset_of ty_x x ->
      []
  | x ->
      Err.fail ~loc_t:loc_x ~has:ty_x ~expected:x

let apply ~loc_f ~loc_x ty_f ty_x =
  let rec aux tyclasses = function
    | Fun (x, eff, t) ->
        let matched_tys = match_tyclass ~loc_x ~tyclasses ~ty_x x in
        let (eff, t) =
          let aux (eff, t) (from, ty) =
            (Effects.replace ~from ~ty eff, replace ~from ~ty t)
          in
          List.fold_left aux (eff, t) matched_tys
        in
        let aux (effects, t) ((name, args), eff) =
          let args =
            let aux = function
              | Param (x, _) as arg ->
                  let eq (y, _) = Ident.Type.equal x y in
                  begin match List.find_pred eq matched_tys with
                  | Some (_, ty) -> Filled ty
                  | None -> arg
                  end
              | Filled _ as arg -> arg
            in
            List.map aux args
          in
          (Effects.empty, TyClass ((name, args), Effects.union effects eff, t))
        in
        List.fold_left aux (eff, t) tyclasses
    | TyClass (x, eff, t) ->
        aux ((x, eff) :: tyclasses) t
    | (Forall _ as ty)
    | (AppOnTy _ as ty)
    | (Ty _ as ty) ->
        Err.function_type ~loc_f ty
    | AbsOnTy _ | Eff _ ->
        assert false
  in
  aux [] ty_f

let apply_ty ~loc_f ~loc_x ~ty_x ~kind_x =
  let rec aux = function
    | Forall (ty, k, res) when Kinds.equal k kind_x ->
        let res = replace ~from:ty ~ty:ty_x res in
        (ty, res)
    | Forall (_, k, _) ->
        kind_missmatch ~loc_x ~has:kind_x ~expected:k
    | TyClass (x, eff, res) ->
        let (ty, res) = aux res in
        (ty, TyClass (x, eff, res))
    | (Fun _ as ty)
    | (AppOnTy _ as ty)
    | (Ty _ as ty) ->
        Err.forall_type ~loc_f ty
    | AbsOnTy _ | Eff _ ->
        assert false
  in
  aux

let rec has_io = function
  | Eff _ | AbsOnTy _ -> assert false
  | AppOnTy _ | Ty _ -> false
  | Fun (_, eff, (AppOnTy _ | Ty _))
  | TyClass (_, eff, (AppOnTy _ | Ty _)) -> Effects.has_io eff
  | Forall (_, _, ret) | TyClass (_, _, ret) | Fun (_, _, ret) -> has_io ret

let rec is_fun = function
  | Fun _ | TyClass _ -> true
  | Forall (_, _, ret) -> is_fun ret
  | Ty _ | AppOnTy _ -> false
  | AbsOnTy _ | Eff _ -> assert false

let is_unit options = function
  | Ty name when Ident.Type.equal name (Builtins.t_unit options) -> true
  | Ty _ | Fun _ | Forall _ | TyClass _ | AppOnTy _ -> false
  | AbsOnTy _ | Eff _ -> assert false
