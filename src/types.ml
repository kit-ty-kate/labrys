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

let fail_not_star ~loc x =
  Err.fail ~loc "The type construct '%s' cannot be used with kind /= '*'" x

let reduce = PrivateTypes.ty_reduce
let replace = PrivateTypes.ty_replace

let switch_pure_arrow (_, pure_arrow) = match pure_arrow with
  | `Allow -> (true, pure_arrow)
  | `Partial | `Forbid -> (false, pure_arrow)

let handle_effects ~loc options (b, _) gamma = function
  | Some eff -> Effects.of_list options gamma eff
  | None when b -> Effects.empty
  | None ->
      (* TODO: Be more precise *)
      Err.fail
        ~loc
        "Pure arrows are forbidden here. If you really want one use the \
         explicit syntax '-[]->' instead"

let rec of_parse_tree_kind ~pure_arrow options gamma = function
  | (loc, UnsugaredTree.Fun (x, eff, y)) ->
      let loc_x = fst x in
      let loc_y = fst y in
      let pa = switch_pure_arrow pure_arrow in
      let (x, k1) = of_parse_tree_kind ~pure_arrow:pa options gamma x in
      let eff = handle_effects ~loc options pure_arrow gamma eff in
      let (y, k2) = of_parse_tree_kind ~pure_arrow options gamma y in
      if Kinds.not_star k1 then
        fail_not_star ~loc:loc_x "->";
      if Kinds.not_star k2 then
        fail_not_star ~loc:loc_y "->";
      (Fun (x, eff, y), Kinds.Star)
  | (_, UnsugaredTree.Ty name) ->
      begin match GammaMap.Types.find_binding name gamma.Gamma.types with
      | (_, Alias (ty, k)) -> (ty, k) (* TODO: Fix variables if already exist *)
      | (name, Abstract k) -> (Ty name, k)
      end
  | (_, UnsugaredTree.Eff effects) ->
      (Eff (Effects.of_list options gamma effects), Kinds.Eff)
  | (_, UnsugaredTree.Forall ((name, k), ret)) ->
      let loc_ret = fst ret in
      let gamma = Gamma.add_type name (Abstract k) gamma in
      let (ret, kx) = of_parse_tree_kind ~pure_arrow options gamma ret in
      if Kinds.not_star kx then
        fail_not_star ~loc:loc_ret "forall";
      (Forall (name, k, ret), Kinds.Star)
  | (loc, UnsugaredTree.TyClass ((name, args), eff, ret)) -> (* TODO: Handle parameters that appears in several classes *)
      let loc_ret = fst ret in
      let (name, gamma, args) =
        let (name, tyclass) =
          GammaMap.TyClass.find_binding name gamma.Gamma.tyclasses
        in
        let loc = Ident.TyClass.loc name in
        let (gamma, args) =
          let f = of_parse_tree_kind ~pure_arrow:(false, `Forbid) options in
          Class.get_params ~loc f gamma args tyclass
        in
        (name, gamma, args)
      in
      let eff = handle_effects ~loc options pure_arrow gamma eff in
      let (ret, kx) = of_parse_tree_kind ~pure_arrow options gamma ret in
      if Kinds.not_star kx then
        fail_not_star ~loc:loc_ret "forall";
      (TyClass ((name, args), eff, ret), Kinds.Star)
  | (_, UnsugaredTree.AbsOnTy ((name, k), ret)) ->
      let gamma = Gamma.add_type name (Abstract k) gamma in
      let (ret, kret) = of_parse_tree_kind ~pure_arrow options gamma ret in
      (AbsOnTy (name, k, ret), Kinds.KFun (k, kret))
  | (loc, UnsugaredTree.AppOnTy (f, x)) ->
      let (f, kf) = of_parse_tree_kind ~pure_arrow options gamma f in
      let pa = switch_pure_arrow pure_arrow in
      let (x, kx) = of_parse_tree_kind ~pure_arrow:pa options gamma x in
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

let of_parse_tree_kind ~pure_arrow options gamma ty =
  let pure_arrow = match pure_arrow with
    | `Allow -> (true, `Allow)
    | `Partial -> (true, `Partial)
    | `Forbid -> (false, `Forbid)
  in
  let (ty, k) = of_parse_tree_kind ~pure_arrow options gamma ty in
  (reduce ty, k)

let of_parse_tree ~pure_arrow options gamma ty =
  let (loc, _) = ty in
  let (ty, k) = of_parse_tree_kind ~pure_arrow options gamma ty in
  if Kinds.not_star k then
    Err.fail ~loc "Values cannot be of kind /= '*'";
  ty

let to_string = PrivateTypes.ty_to_string

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

  let name_tyclass_missmatch ~has ~expected =
    Err.fail
      ~loc:(Ident.TyClass.loc has)
      "Typeclass name missmatch. Has '%s' but expected '%s'"
      (Ident.TyClass.to_string has)
      (Ident.TyClass.to_string expected)

  let tyclass_type ~loc ty =
    Err.fail
      ~loc
      "Error: This expression has type '%s'. \
       This is not a typeclass abstraction; it cannot be applied."
      (to_string ty)

  let args_tyclass_missmatch tyclass ~expected ~has =
    let param_to_string = function
      | Param (name, _) -> Ident.Type.to_string name
      | Filled ty -> to_string ty
    in
    Err.fail
      ~loc:(Ident.TyClass.loc tyclass)
      "Tyclass arguments missmatch. Has '%s %s' but expected '%s %s'"
      (Ident.TyClass.to_string tyclass)
      (String.concat " " (List.map param_to_string has))
      (Ident.TyClass.to_string tyclass)
      (String.concat " " (List.map param_to_string expected))
end

let match_tyclass ~loc_x ~tyclasses =
  let is_tyclass name =
    let aux = function
      | Param (x, _) -> Ident.Type.equal name x
      | Filled _ -> false
    in
    let aux ((_, args), _) = List.exists aux args in
    List.exists aux tyclasses
  in
  let rec aux x ~ty_x = match x, ty_x with
    | Ty name, _ when is_tyclass name ->
        ([(name, ty_x)], ty_x)
    | Ty _, Ty _ ->
        ([], x)
    | Fun (x1, eff1, y1), Fun (x2, eff2, y2) ->
        let (l1, x1) = aux x1 ~ty_x:x2 in
        let (l2, eff1) = Effects.match_tyclass ~is_tyclass eff1 ~eff_x:eff2 in
        let (l3, y1) = aux y1 ~ty_x:y2 in
        (l1 @ l2 @ l3, Fun (x1, eff1, y1))
    | Forall (name1, k1, t1), Forall (_, _, t2) ->
        let (l, t1) = aux t1 ~ty_x:t2 in
        (l, Forall (name1, k1, t1))
    | TyClass (tyclass1, eff1, t1), TyClass (_, eff2, t2) ->
        let (l1, eff1) = Effects.match_tyclass ~is_tyclass eff1 ~eff_x:eff2 in
        let (l2, t1) = aux t1 ~ty_x:t2 in
        (l1 @ l2, TyClass (tyclass1, eff1, t1))
    | AppOnTy (f1, x1), AppOnTy (f2, x2) ->
        let (l1, f1) = aux f1 ~ty_x:f2 in
        let (l2, x1) = aux x1 ~ty_x:x2 in
        (l1 @ l2, AppOnTy (f1, x1))
    | Ty _, _
    | Fun _, _
    | Forall _, _
    | TyClass _, _
    | AppOnTy _, _ ->
        Err.fail ~loc_t:loc_x ~has:ty_x ~expected:x
    | Eff _, _ | AbsOnTy _, _ ->
        assert false
  in
  aux

let rec remove_filled_tyclasses = function
  | Ty _ | Fun _ | Forall _ | AppOnTy _ as t ->
      (t, Effects.empty)
  | TyClass ((_, args), eff, t) as t' ->
      let is_filled = function
        | Param _ -> false
        | Filled _ -> true
      in
      if List.for_all is_filled args then
        let (t, eff') = remove_filled_tyclasses t in
        (t, Effects.union eff eff')
      else
        (t', Effects.empty)
  | AbsOnTy _ | Eff _ ->
      assert false

let apply ~loc_f ~loc_x ty_f ty_x =
  let rec aux tyclasses = function
    | Fun (x, eff, t) ->
        let (matched_tys, x) = match_tyclass ~loc_x ~tyclasses ~ty_x x in
        (* TODO: Check if names matches the same type in matched_tys *)
        if not (is_subset_of ty_x x) then
          Err.fail ~loc_t:loc_x ~has:ty_x ~expected:x;
        let (eff, t) =
          let aux (eff, t) (from, ty) =
            (Effects.replace ~from ~ty eff, replace ~from ~ty t)
          in
          List.fold_left aux (eff, t) matched_tys
        in
        let (eff, t, tyclasses) =
          let aux (effects, t, tyclasses) ((name, args), eff) =
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
            let effects = Effects.union effects eff in
            let tyclass = (name, args) in
            (Effects.empty, TyClass (tyclass, effects, t), tyclass :: tyclasses)
          in
          List.fold_left aux (eff, t, []) tyclasses
        in
        let (t, eff') = remove_filled_tyclasses t in
        (Effects.union eff eff', t, tyclasses)
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
        Kinds.Err.fail ~loc:loc_x ~has:kind_x ~expected:k
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

let apply_tyclass ty tyclass args = match ty with
  | TyClass ((name, args'), eff, res) ->
      if not (Ident.TyClass.equal name tyclass) then
        Err.name_tyclass_missmatch ~has:tyclass ~expected:name;
      if not (PrivateTypes.tyclass_args_equal args args') then
        Err.args_tyclass_missmatch tyclass ~expected:args ~has:args';
      let res =
        try
          let aux res ty1 ty2 = match ty1, ty2 with
            | Param (x1, _), Param (x2, _) -> replace ~from:x2 ~ty:(Ty x1) res
            | Filled _, Filled _ -> res
            | Filled ty, Param (from, _) -> replace ~from ~ty res
            | Param _, _ -> assert false
          in
          List.fold_left2 aux res args args'
        with
        | Invalid_argument _ -> assert false
      in
      (res, eff)
  | Ty _ | Fun _ | Forall _ | AppOnTy _ as ty ->
      Err.tyclass_type ~loc:(Ident.TyClass.loc tyclass) ty
  | AbsOnTy _ | Eff _ ->
      assert false

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

let get_tys_filled args =
  let rec aux res = function
    | Filled ty :: xs -> aux (ty :: res) xs
    | Param _ :: _ -> []
    | [] -> res
  in
  if List.is_empty args then
    assert false;
  List.rev (aux [] args)

let tyclass_wrap tyclass params ty =
  let params = List.map (fun x -> Param x) params in
  TyClass ((tyclass, params), Effects.empty, ty)
