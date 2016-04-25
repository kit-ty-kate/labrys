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

open Containers
open Monomorphic.None

type name = Ident.Type.t
type tyvar_name = Ident.TypeVar.t

type t = PrivateTypes.t =
  | Ty of name
  | TyVar of tyvar_name
  | Eff of PrivateTypes.effects
  | Fun of (t * PrivateTypes.effects * t)
  | Forall of (tyvar_name * Kinds.t * t)
  | TyClass of ((Ident.TyClass.t * Kinds.t GammaMap.TypeVar.t * t list) * Effects.t * t)
  | AbsOnTy of (tyvar_name * Kinds.t * t)
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
      begin match GammaMap.Types.find name gamma.Gamma.types with
      | Alias (ty, k) -> (ty, k) (* TODO: Fix variables if already exist *)
      | Abstract k -> (Ty name, k)
      end
  | (_, UnsugaredTree.TyVar name) ->
      let k = GammaMap.TypeVar.find name gamma.Gamma.type_vars in
      (TyVar name, k)
  | (_, UnsugaredTree.Eff effects) ->
      (Eff (Effects.of_list options gamma effects), Kinds.Eff)
  | (_, UnsugaredTree.Forall ((name, k), ret)) ->
      let loc_ret = fst ret in
      let gamma = Gamma.add_type_var name k gamma in
      let (ret, kx) = of_parse_tree_kind ~pure_arrow options gamma ret in
      if Kinds.not_star kx then
        fail_not_star ~loc:loc_ret "forall";
      (Forall (name, k, ret), Kinds.Star)
  | (loc, UnsugaredTree.TyClass ((name, tyvars, args), eff, ret)) ->
      let loc_ret = fst ret in
      let (gamma, tyvars, args) =
        let tyclass = GammaMap.TyClass.find name gamma.Gamma.tyclasses in
        let tyvars =
          let aux acc (name, k) = GammaMap.TypeVar.add name k acc in
          List.fold_left aux GammaMap.TypeVar.empty tyvars
        in
        let loc = Ident.TyClass.loc name in
        let (gamma, args) =
          let f = of_parse_tree_kind ~pure_arrow:(false, `Forbid) options in
          Class.get_params ~loc f gamma tyvars args tyclass
        in
        (gamma, tyvars, args)
      in
      let eff = handle_effects ~loc options pure_arrow gamma eff in
      let (ret, kx) = of_parse_tree_kind ~pure_arrow options gamma ret in
      if Kinds.not_star kx then
        fail_not_star ~loc:loc_ret "forall";
      (TyClass ((name, tyvars, args), eff, ret), Kinds.Star)
  | (_, UnsugaredTree.AbsOnTy ((name, k), ret)) ->
      let gamma = Gamma.add_type_var name k gamma in
      let (ret, kret) = of_parse_tree_kind ~pure_arrow options gamma ret in
      (AbsOnTy (name, k, ret), Kinds.KFun (k, kret))
  | (loc, UnsugaredTree.AppOnTy (f, x)) ->
      let (f, kf) = of_parse_tree_kind ~pure_arrow options gamma f in
      let pa = switch_pure_arrow pure_arrow in
      let (x, kx) = of_parse_tree_kind ~pure_arrow:pa options gamma x in
      let k =
        match kf with
        | Kinds.KFun (p, r) when Kinds.equal p kx ->
            r
        | Kinds.KFun _ | Kinds.Eff | Kinds.Star as k ->
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
  | AppOnTy _ | AbsOnTy _ | Eff _ | Ty _ | TyVar _ -> 0
  | Forall (_, _, t) -> size t

let rec is_value = function
  | Ty _ | TyVar _ -> true
  | Eff _ | AbsOnTy _ -> assert false
  | Fun _ | Forall _ | TyClass _ -> false
  | AppOnTy (t, _) -> is_value t

let rec head = function
  | Ty name -> (name, [])
  | TyVar name ->
      Err.fail
        ~loc:(Ident.TypeVar.loc name)
        "Cannot match over a type variable '%s'"
        (Ident.TypeVar.to_string name)
  | Eff _
  | Fun _
  | Forall _
  | TyClass _
  | AbsOnTy _ -> assert false
  | AppOnTy (t, y) ->
      let (x, xs) = head t in
      (x, y :: xs)

module TyErr = struct
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
end

let match_tyclass ~loc_x ~tyclasses ~tyclasses_x =
  let is_tyclass name = GammaSet.TypeVar.mem name tyclasses in
  let is_tyclass_x name = GammaSet.TypeVar.mem name tyclasses_x in
  let rec aux x ~ty_x = match x, ty_x with
    | TyVar name1, TyVar name2 when is_tyclass name1 && is_tyclass_x name2 ->
        Err.fail
          ~loc:loc_x
          "Typeclass identifiers collition with '%s' and '%s'. \
           Impossible to match an instance"
          (to_string x)
          (to_string ty_x)
    | TyVar name, _ when is_tyclass name ->
        ([(name, ty_x)], ty_x, [], ty_x)
    | _, TyVar name when is_tyclass_x name ->
        ([], x, [(name, x)], x)
    | Ty _, Ty _ | TyVar _, TyVar _ ->
        ([], x, [], ty_x)
    | Fun (x1, eff1, y1), Fun (x2, eff2, y2) ->
        let (l1, x1, l1', x2) = aux x1 ~ty_x:x2 in
        let (l2, eff1, l2', eff2) = Effects.match_tyclass ~is_tyclass ~is_tyclass_x eff1 ~eff_x:eff2 in
        let (l3, y1, l3', y2) = aux y1 ~ty_x:y2 in
        (l1 @ l2 @ l3, Fun (x1, eff1, y1), l1' @ l2' @ l3', Fun (x2, eff2, y2))
    | Forall (name1, k1, t1), Forall (name2, k2, t2) ->
        let (l, t1, l', t2) = aux t1 ~ty_x:t2 in
        (l, Forall (name1, k1, t1), l', Forall (name2, k2, t2))
    | TyClass (tyclass1, eff1, t1), TyClass (tyclass2, eff2, t2) ->
        let (l1, eff1, l1', eff2) = Effects.match_tyclass ~is_tyclass ~is_tyclass_x eff1 ~eff_x:eff2 in
        let (l2, t1, l2', t2) = aux t1 ~ty_x:t2 in
        (l1 @ l2, TyClass (tyclass1, eff1, t1), l1' @ l2', TyClass (tyclass2, eff2, t2))
    | AppOnTy (f1, x1), AppOnTy (f2, x2) ->
        let (l1, f1, l1', f2) = aux f1 ~ty_x:f2 in
        let (l2, x1, l2', x2) = aux x1 ~ty_x:x2 in
        (l1 @ l2, AppOnTy (f1, x1), l1' @ l2', AppOnTy (f2, x2))
    | Ty _, _
    | TyVar _, _
    | Fun _, _
    | Forall _, _
    | TyClass _, _
    | AppOnTy _, _ ->
        TyErr.fail ~loc_t:loc_x ~has:ty_x ~expected:x
    | Eff _, _ | AbsOnTy _, _ ->
        assert false
  in
  aux

(* NOTE: returns a reversed tyclasses list *)
let extract_tyclasses =
  let add_to_set tyclasses (_, tyvars, _) =
    GammaMap.TypeVar.fold (fun k _ -> GammaSet.TypeVar.add k) tyvars tyclasses
  in
  let rec aux tyclasses set = function
    | TyClass (x, eff, t) -> aux ((x, eff) :: tyclasses) (add_to_set set x) t
    | Ty _ | TyVar _ | Fun _ | Forall _ | AppOnTy _ as t -> (tyclasses, set, t)
    | AbsOnTy _ | Eff _ -> assert false
  in
  aux [] GammaSet.TypeVar.empty

let match_tyclass ~loc_x ~tyclasses x ~ty_x =
  let (tyclasses_x_list, tyclasses_x, ty_x') = extract_tyclasses ty_x in
  let ((matched, ty, matched_x, ty_x), tyclasses_x_list) =
    let (_, _, matched_x, _) as res =
      match_tyclass ~loc_x ~tyclasses ~tyclasses_x x ~ty_x:ty_x'
    in
    if Int.(List.length matched_x = GammaSet.TypeVar.cardinal tyclasses_x) then
      (res, tyclasses_x_list)
    else
      let tyclasses_x = GammaSet.TypeVar.empty in
      (match_tyclass ~loc_x ~tyclasses ~tyclasses_x x ~ty_x, [])
  in
  let rec aux acc = function
    | ((x, ty) as x')::xs ->
        let (l, xs) =
          let aux ((y, ty) as pair) =
            if Ident.TypeVar.equal x y then `Left ty else `Right pair
          in
          List.partition_map aux xs
        in
        if not (List.for_all (equal ty) l) then
          Err.fail ~loc:loc_x "Type constraints doesn't match"; (* TODO: Improve error message *)
        aux (x' :: acc) xs
    | [] ->
        acc
  in
  (aux [] matched, ty, aux [] matched_x, ty_x, tyclasses_x_list)

(* NOTE: take a reversed tyclasses list *)
let reconstruct_ty_typeclasses matched_tys tyclasses_list t eff =
  let aux (effects, t) ((name, tyvars, args), eff) =
    let tyvars =
      let aux x k tyvars =
        let eq (y, _) = Ident.TypeVar.equal x y in
        if List.exists eq matched_tys then
          tyvars
        else
          GammaMap.TypeVar.add x k tyvars
      in
      GammaMap.TypeVar.fold aux tyvars GammaMap.TypeVar.empty
    in
    let args =
      let aux t =
        let aux t (from, ty) = replace ~from ~ty t in
        List.fold_left aux t matched_tys
      in
      List.map aux args
    in
    let effects = Effects.union effects eff in
    (Effects.empty, TyClass ((name, tyvars, args), effects, t))
  in
  let (eff, t) =
    let aux (eff, t) (from, ty) =
      (Effects.replace ~from ~ty eff, replace ~from ~ty t)
    in
    List.fold_left aux (eff, t) matched_tys
  in
  List.fold_left aux (eff, t) tyclasses_list

let apply ~loc_f ~loc_x ty_f ty_x =
  let (tyclasses_list, tyclasses, ty_f) = extract_tyclasses ty_f in
  match ty_f with
  | Fun (x, eff, t) ->
      let (matched_tys, x, matched_tys_x, ty_x, tyclasses_x_list) =
        match_tyclass ~loc_x ~tyclasses ~ty_x x
      in
      if not (is_subset_of ty_x x) then
        TyErr.fail ~loc_t:loc_x ~has:ty_x ~expected:x;
      let (eff, t) =
        reconstruct_ty_typeclasses matched_tys tyclasses_list t eff
      in
      let (eff_x, ty_x) =
        reconstruct_ty_typeclasses matched_tys_x tyclasses_x_list ty_x Effects.empty
      in
      if not (Effects.is_empty eff_x) then
        assert false;
      (eff, t, ty_x)
  | Forall _ | AppOnTy _ | Ty _ | TyVar _ as ty ->
      TyErr.function_type ~loc_f ty
  | TyClass _ | AbsOnTy _ | Eff _ ->
      assert false

let apply_ty ~loc_f ~loc_x ~ty_x ~kind_x =
  let rec aux = function
    | Forall (ty, k, res) when Kinds.equal k kind_x ->
        replace ~from:ty ~ty:ty_x res
    | Forall (_, k, _) ->
        Kinds.Err.fail ~loc:loc_x ~has:kind_x ~expected:k
    | TyClass (x, eff, res) ->
        TyClass (x, eff, aux res)
    | Fun _ | AppOnTy _ | Ty _ | TyVar _ as ty ->
        TyErr.forall_type ~loc_f ty
    | AbsOnTy _ | Eff _ ->
        assert false
  in
  aux

let unify_tyclass_one ~loc_x ~is_new_tyvar =
  let rec aux x ~ty_x = match x, ty_x with
    | TyVar name, _ when is_new_tyvar name ->
        [(name, ty_x)]
    | Ty _, Ty _ | TyVar _, TyVar _ ->
        []
    | Eff eff1, Eff eff2 ->
        Effects.unify_tyclass ~is_new_tyvar eff1 ~eff_x:eff2
    | Fun (x1, eff1, y1), Fun (x2, eff2, y2) ->
        let l1 = aux x1 ~ty_x:x2 in
        let l2 = Effects.unify_tyclass ~is_new_tyvar eff1 ~eff_x:eff2 in
        let l3 = aux y1 ~ty_x:y2 in
        l1 @ l2 @ l3
    | Forall (name1, k1, t1), Forall (name2, k2, t2) ->
        aux t1 ~ty_x:t2
    | TyClass (tyclass1, eff1, t1), TyClass (tyclass2, eff2, t2) ->
        let l1 = Effects.unify_tyclass ~is_new_tyvar eff1 ~eff_x:eff2 in
        let l2 = aux t1 ~ty_x:t2 in
        l1 @ l2
    | AppOnTy (f1, x1), AppOnTy (f2, x2) ->
        let l1 = aux f1 ~ty_x:f2 in
        let l2 = aux x1 ~ty_x:x2 in
        l1 @ l2
    | AbsOnTy (_, _, t1), AbsOnTy (_, _, t2) ->
        aux t1 ~ty_x:t2
    | (Ty _ | Eff _ | TyVar _ | Fun _ | Forall _ | TyClass _ | AppOnTy _ | AbsOnTy _), _ ->
        TyErr.fail ~loc_t:loc_x ~has:ty_x ~expected:x
  in
  aux

(* TODO: Equality *)
let unify_tyclass ~loc_x ~is_new_tyvar l1 l2 =
  try
    let aux acc x ty_x =
      unify_tyclass_one ~loc_x ~is_new_tyvar x ~ty_x @ acc
    in
    List.fold_left2 aux [] l1 l2
  with
  | Invalid_argument _ -> assert false

let apply_tyclass ~loc_x ty tyclass args = match ty with
  | TyClass ((name, tyvars, args'), eff, res) ->
      if not (Ident.TyClass.equal name tyclass) then
        TyErr.name_tyclass_missmatch ~has:tyclass ~expected:name;
      let is_new_tyvar name = GammaMap.TypeVar.mem name tyvars in
      let tyvars = unify_tyclass ~loc_x ~is_new_tyvar args' args in
      let aux (res, eff) = function
        | (from, ty)::tyvars ->
            if List.exists (fun (name, _) -> Ident.TypeVar.equal from name) tyvars then
              Err.fail ~loc:loc_x "Type constraints doesn't match"; (* TODO: Improve error message *)
            (replace ~from ~ty res, Effects.replace ~from ~ty eff)
        | [] ->
            (res, eff)
      in
      aux (res, eff) tyvars
  | Ty _ | TyVar _ | Fun _ | Forall _ | AppOnTy _ as ty ->
      TyErr.tyclass_type ~loc:(Ident.TyClass.loc tyclass) ty
  | AbsOnTy _ | Eff _ ->
      assert false

let rec has_io options = function
  | Eff _ | AbsOnTy _ -> assert false
  | AppOnTy _ | Ty _ | TyVar _ -> false
  | Fun (_, eff, (AppOnTy _ | Ty _))
  | TyClass (_, eff, (AppOnTy _ | Ty _)) -> Effects.has_io options eff
  | Forall (_, _, ret)
  | TyClass (_, _, ret)
  | Fun (_, _, ret) -> has_io options ret

let rec is_fun = function
  | Fun _ | TyClass _ -> true
  | Forall (_, _, ret) -> is_fun ret
  | Ty _ | TyVar _ | AppOnTy _ -> false
  | AbsOnTy _ | Eff _ -> assert false

let is_unit options = function
  | Ty name when Ident.Type.equal name (Builtins.unit options) -> true
  | Ty _ | TyVar _ | Fun _ | Forall _ | TyClass _ | AppOnTy _ -> false
  | AbsOnTy _ | Eff _ -> assert false

let tyclass_wrap tyclass params ty =
  let (tyvars, args) =
    let aux (tyvars, args) (name, k) =
      (GammaMap.TypeVar.add name k tyvars, TyVar name :: args)
    in
    List.fold_left aux (GammaMap.TypeVar.empty, []) params
  in
  let args = List.rev args in
  TyClass ((tyclass, tyvars, args), Effects.empty, ty)

let contains_free_tyvars =
  let rec aux gammaTV = function
    | Ty _ | Eff _ -> false
    | TyVar name when GammaSet.TypeVar.mem name gammaTV -> false
    | TyVar _ -> true
    | Fun (x, eff, y) ->
        aux gammaTV x
        || Effects.contains_free_tyvars gammaTV eff
        || aux gammaTV y
    | Forall (name, _, t) | AbsOnTy (name, _, t) ->
        aux (GammaSet.TypeVar.add name gammaTV) t
    | TyClass ((_, tyvars, args), eff, t) ->
        let gammaTV =
          GammaSet.TypeVar.union (GammaMap.TypeVar.to_set tyvars) gammaTV
        in
        List.exists (aux gammaTV) args
        || Effects.contains_free_tyvars gammaTV eff
        || aux gammaTV t
    | AppOnTy (f, x) -> aux gammaTV f || aux gammaTV x
  in
  aux GammaSet.TypeVar.empty

let rec extract_filled_tyclasses = function
  | TyClass ((tyclass, tyvars, args), eff, t') as t ->
      let (tyclasses, eff', t') = extract_filled_tyclasses t' in
      if List.exists contains_free_tyvars args then
        (None :: tyclasses, eff', t)
      else
        (Some (tyclass, args) :: tyclasses, Effects.union eff eff', t')
  | Ty _ | TyVar _ | Fun _ | Forall _ | AppOnTy _ as t -> ([], Effects.empty, t)
  | Eff _ | AbsOnTy _ -> assert false

let rec is_bound name = function
  | Ty _ | TyVar _ | Eff _ ->
      false
  | Fun (x, _, y) | AppOnTy (x, y) ->
      is_bound name x || is_bound name y
  | Forall (name', _, t) | AbsOnTy (name', _, t) ->
      Ident.TypeVar.equal name name' || is_bound name t
  | TyClass ((_, tyvars, _), _, t) ->
      GammaMap.TypeVar.mem name tyvars || is_bound name t

let check_bound name t =
  (* TODO: Avoid this *)
  if is_bound name t then
    Err.fail
      ~loc:(Ident.TypeVar.loc name)
      "TEMPORARY ERROR: Cannot bind this name here as it is already used later \
       in the type"

let forall (name, k, t) =
  check_bound name t;
  Forall (name, k, t)

let tyclass ((tyclass, tyvars, args), eff, t) =
  let aux name _ = check_bound name t in
  GammaMap.TypeVar.iter aux tyvars;
  TyClass ((tyclass, tyvars, args), eff, t)

let ty ~loc gamma name =
  if not (GammaMap.Types.mem name gamma.Gamma.types) then
    Err.fail ~loc "Type '%s' is not defined yet" (Ident.Type.to_string name);
  Ty name
