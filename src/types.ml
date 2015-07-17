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

type t = PrivateTypes.t =
  | Ty of name
  | Eff of PrivateTypes.effects
  | Fun of (t * PrivateTypes.effects * t)
  | Forall of (name * Kinds.t * t)
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
    | AbsOnTy (x, k, t) -> AbsOnTy (x, k, aux t)
    | AppOnTy (f, x) ->
        let x = aux x in
        begin match aux f with
        | AbsOnTy (from', _, t) -> replace ~from:from' ~ty:x t
        | (Ty _ as f)
        | (AppOnTy _ as f) -> AppOnTy (f, x)
        | Eff _ | Fun _ | Forall _ -> assert false
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

let rec of_parse_tree_kind ~pure_arrow options gammaT gammaExn = function
  | (loc, UnsugaredTree.Fun (x, eff, y)) ->
      let loc_x = fst x in
      let loc_y = fst y in
      let pa = switch_pure_arrow pure_arrow in
      let (x, k1) = of_parse_tree_kind ~pure_arrow:pa options gammaT gammaExn x in
      let eff = handle_effects ~loc options pure_arrow gammaExn gammaT eff in
      let (y, k2) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn y in
      if Kinds.not_star k1 then
        fail_not_star ~loc:loc_x "->";
      if Kinds.not_star k2 then
        fail_not_star ~loc:loc_y "->";
      (Fun (x, eff, y), Kinds.Star)
  | (_, UnsugaredTree.Ty name) ->
      begin match GammaMap.Types.fill_module name gammaT with
      | (_, Alias (ty, k)) -> (ty, k)
      | (name, Abstract k) -> (Ty name, k)
      end
  | (_, UnsugaredTree.Eff effects) ->
      (Eff (Effects.of_list options gammaExn gammaT effects), Kinds.Eff)
  | (_, UnsugaredTree.Forall ((name, k), ret)) ->
      let loc_ret = fst ret in
      let gammaT = GammaMap.Types.add name (Abstract k) gammaT in
      let (ret, kx) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn ret in
      if Kinds.not_star kx then
        fail_not_star ~loc:loc_ret "forall";
      (Forall (name, k, ret), Kinds.Star)
  | (_, UnsugaredTree.ForallTyClass ((name, args), ret)) ->
      assert false
  | (_, UnsugaredTree.AbsOnTy ((name, k), ret)) ->
      let gammaT = GammaMap.Types.add name (Abstract k) gammaT in
      let (ret, kret) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn ret in
      (AbsOnTy (name, k, ret), Kinds.KFun (k, kret))
  | (_, UnsugaredTree.AppOnTy ((_, UnsugaredTree.AbsOnTy ((name, k), t)), x)) ->
      let loc_x = fst x in
      let pa = switch_pure_arrow pure_arrow in
      let (x, kx) = of_parse_tree_kind ~pure_arrow:pa options gammaT gammaExn x in
      if not (Kinds.equal k kx) then
        kind_missmatch ~loc_x ~has:kx ~expected:k;
      let gammaT = GammaMap.Types.add name (Abstract k) gammaT in
      let (t, kt) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn t in
      (replace ~from:name ~ty:x t, kt)
  | (loc, UnsugaredTree.AppOnTy (f, x)) ->
      let (f, kf) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn f in
      let pa = switch_pure_arrow pure_arrow in
      let (x, kx) = of_parse_tree_kind ~pure_arrow:pa options gammaT gammaExn x in
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

let of_parse_tree_kind ~pure_arrow gammaT ty =
  let pure_arrow = match pure_arrow with
    | `Allow -> (true, pure_arrow)
    | `Partial -> (true, pure_arrow)
    | `Forbid -> (false, pure_arrow)
  in
  of_parse_tree_kind ~pure_arrow gammaT ty

let of_parse_tree ~pure_arrow options gammaT gammaExn ty =
  let (loc, _) = ty in
  let (ty, k) = of_parse_tree_kind ~pure_arrow options gammaT gammaExn ty in
  if Kinds.not_star k then
    Err.fail ~loc "Values cannot be of kind /= '*'";
  ty

let rec to_string = function
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
    | Eff x, Eff x' ->
        eff_eq eq_list x x'
    | Fun (param, eff1, res), Fun (param', eff2, res') ->
        aux eq_list (param, param')
        && eff_eq eq_list eff1 eff2
        && aux eq_list (res, res')
    | AppOnTy (f, x), AppOnTy (f', x') ->
        aux eq_list (f, f') && aux eq_list (x, x')
    | AbsOnTy (name1, k1, t), AbsOnTy (name2, k2, t') when Kinds.equal k1 k2 ->
        aux ((name1, name2) :: eq_list) (t, t')
    | Forall (name1, k1, t), Forall (name2, k2, t') when Kinds.equal k1 k2 ->
        aux ((name1, name2) :: eq_list) (t, t')
    | AppOnTy _, _
    | AbsOnTy _, _
    | Forall _, _
    | Ty _, _
    | Eff _, _
    | Fun _, _ -> false
  in
  aux [] (x, y)

let is_subset_of = equal Effects.is_subset_of

let equal = equal Effects.equal

let rec size = function
  | Fun (_, _, t) -> succ (size t)
  | AppOnTy _
  | AbsOnTy _
  | Eff _
  | Ty _ -> 0
  | Forall (_, _, t) -> size t

let rec is_value = function
  | Ty _ -> true
  | Eff _
  | AbsOnTy _ -> assert false
  | Fun _
  | Forall _ -> false
  | AppOnTy (t, _) -> is_value t

let rec head = function
  | Ty name -> (name, [])
  | Eff _
  | Fun _
  | Forall _
  | AbsOnTy _ -> assert false
  | AppOnTy (t, y) ->
      let (x, xs) = head t in
      (x, y :: xs)

let remove_module_aliases =
  let rec aux vars = function
    | Ty name when not (List.mem name vars) ->
        Ty (Ident.Type.remove_aliases name)
    | Ty name ->
        Ty name
    | Eff effects ->
        Eff (Effects.remove_module_aliases vars effects)
    | Fun (x, eff, y) ->
        Fun (aux vars x, Effects.remove_module_aliases vars eff, aux vars y)
    | Forall (name, k, t) ->
        Forall (name, k, aux (name :: vars) t)
    | AbsOnTy (name, k, t) ->
        AbsOnTy (name, k, aux (name :: vars) t)
    | AppOnTy (x, y) ->
        AppOnTy (aux vars x, aux vars y)
  in
  aux []

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

let apply ~loc_f = function
  | Fun x ->
      x
  | (Forall _ as ty)
  | (AppOnTy _ as ty)
  | (Ty _ as ty) ->
      Err.function_type ~loc_f ty
  | AbsOnTy _ | Eff _ ->
      assert false

let apply_ty ~loc_f ~loc_x ~ty_x ~kind_x = function
  | Forall (ty, k, res) when Kinds.equal k kind_x ->
      let res = replace ~from:ty ~ty:ty_x res in
      (ty, res)
  | Forall (_, k, _) ->
      kind_missmatch ~loc_x ~has:kind_x ~expected:k
  | (Fun _ as ty)
  | (AppOnTy _ as ty)
  | (Ty _ as ty) ->
      Err.forall_type ~loc_f ty
  | AbsOnTy _ | Eff _ ->
      assert false

let rec has_io = function
  | Eff _ | AbsOnTy _ -> assert false
  | AppOnTy _ | Ty _ -> false
  | Fun (_, eff, (AppOnTy _ | Ty _)) -> Effects.has_io eff
  | Forall (_, _, ret) | Fun (_, _, ret) -> has_io ret

let rec is_fun = function
  | Fun _ -> true
  | Forall (_, _, ret) -> is_fun ret
  | Ty _ | AppOnTy _ -> false
  | AbsOnTy _ | Eff _ -> assert false

let is_unit options = function
  | Ty name when Ident.Type.equal name (Builtins.t_unit options) -> true
  | Ty _ | Fun _ | Forall _ | AppOnTy _ -> false
  | AbsOnTy _ | Eff _ -> assert false
