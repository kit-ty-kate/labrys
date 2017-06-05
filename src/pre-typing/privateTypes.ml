(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

module Exn_set = Utils.EqSet(Ident.Exn)
module Variables = Utils.EqSet(Ident.TypeVar)
module Effects = Utils.EqSet(Ident.Type)

type name = Ident.Name.t
type ty_name = Ident.Type.t
type tyvar_name = Ident.TypeVar.t

type effects =
  { variables : Variables.t
  ; effects : Effects.t
  ; exns : Exn_set.t
  }

type t =
  | Ty of ty_name
  | TyVar of tyvar_name
  | Eff of effects
  | Fun of (t * effects * t)
  | Forall of (tyvar_name * Kinds.t * t)
  | TyClass of ((Ident.TyClass.t * Kinds.t GammaMap.TypeVar.t * t list) * effects * t)
  | AbsOnTy of (tyvar_name * Kinds.t * t)
  | AppOnTy of (t * t)

type visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

let fmt = Printf.sprintf

let var_eq list_eq x y =
  let aux k =
    let mem k = Variables.mem k y in
    match List.find_pred (fun (k', _) -> Ident.TypeVar.equal k k') list_eq with
    | Some (_, k) -> mem k
    | None -> mem k
  in
  Variables.for_all aux x

let eff_is_subset_of list_eq x y =
  var_eq list_eq x.variables y.variables
  && Effects.subset x.effects y.effects
  && Exn_set.subset x.exns y.exns

let eff_union x y =
  let variables = Variables.union x.variables y.variables in
  let effects = Effects.union x.effects y.effects in
  let exns = Exn_set.union x.exns y.exns in
  {variables; effects; exns}

let eff_union_ty' self = function
  | Ty name -> {self with effects = Effects.add name self.effects}
  | TyVar name -> {self with variables = Variables.add name self.variables}
  | Eff eff -> eff_union self eff
  | Fun _ | Forall _ | TyClass _ | AbsOnTy _ | AppOnTy _ -> assert false

let eff_union_ty ~from self =
  let self = {self with variables = Variables.remove from self.variables} in
  eff_union_ty' self

let eff_replace ~from ~ty self =
  let aux name self =
    if Ident.TypeVar.equal name from then
      let self = {self with variables = Variables.remove name self.variables} in
      eff_union_ty ~from self ty
    else
      self
  in
  Variables.fold aux self.variables self

let ty_is_subset_of' eq_list x y =
  let rec aux eq_list = function
    | Ty x, Ty x' ->
        let eq = Ident.Type.equal in
        eq x x'
    | TyVar x, TyVar x' ->
        let eq = Ident.TypeVar.equal in
        eq x x' || List.exists (fun (y, y') -> eq x y && eq x' y') eq_list
    | Eff x, Eff x' ->
        eff_is_subset_of eq_list x x'
    | Fun (param, eff1, res), Fun (param', eff2, res') ->
        aux eq_list (param, param')
        && eff_is_subset_of eq_list eff1 eff2
        && aux eq_list (res, res')
    | AppOnTy (f, x), AppOnTy (f', x') ->
        aux eq_list (f, f') && aux eq_list (x, x')
    | AbsOnTy (name1, k1, t), AbsOnTy (name2, k2, t') when Kinds.equal k1 k2 ->
        aux ((name1, name2) :: eq_list) (t, t')
    | Forall (name1, k1, t), Forall (name2, k2, t') when Kinds.equal k1 k2 ->
        aux ((name1, name2) :: eq_list) (t, t')
    | TyClass ((name1, tyvars1, args1), eff1, t), TyClass ((name2, tyvars2, args2), eff2, t') ->
        begin try
          let l =
            let aux acc ty1 ty2 =
              let eq_list =
                let conv = GammaMap.TypeVar.bindings in
                let aux (name1, _) (name2, _) = (name1, name2) in
                List.map2 aux (conv tyvars1) (conv tyvars2) @ eq_list
              in
              if not (aux eq_list (ty1, ty2)) then
                raise Not_found;
              acc
            in
            List.fold_left2 aux [] args1 args2
          in
          (* NOTE: no need to check kinds as it is already checked *)
          Ident.TyClass.equal name1 name2
          && eff_is_subset_of eq_list eff1 eff2
          && aux (l @ eq_list) (t, t')
        with Not_found | Invalid_argument _ ->
          false
        end
    | AppOnTy _, _
    | AbsOnTy _, _
    | Forall _, _
    | TyClass _, _
    | Ty _, _
    | TyVar _, _
    | Eff _, _
    | Fun _, _ -> false
  in
  aux eq_list (x, y)

let ty_equal' l x y = ty_is_subset_of' l x y && ty_is_subset_of' l y x
let ty_equal = ty_equal' []

let ty_is_subset_of = ty_is_subset_of' []

let tyclass_args_equal args1 args2 =
  (* TODO: Check if correct. See comment above *)
  let aux (tyvars1, ty1) (tyvars2, ty2) =
    let eq_list =
      List.fold_left2 (fun acc x y -> (x, y) :: acc) [] tyvars1 tyvars2
    in
    ty_equal' eq_list ty1 ty2
  in (* TODO: DEBUJIN ? *)
  try List.for_all2 aux args1 args2 with Invalid_argument _ -> assert false

let eff_is_empty {variables; effects; exns} =
  Variables.is_empty variables
  && Effects.is_empty effects
  && Exn_set.is_empty exns

let eff_to_string self =
  let variables =
    let aux name acc = Ident.TypeVar.to_string name :: acc in
    Variables.fold aux self.variables []
  in
  let effects =
    let aux name acc = Ident.Type.to_string name :: acc in
    Effects.fold aux self.effects []
  in
  let exns =
    if Exn_set.is_empty self.exns then
      []
    else
      let aux name acc = Ident.Exn.to_string name :: acc in
      [fmt "Exn [%s]" (String.concat " | " (Exn_set.fold aux self.exns []))]
  in
  fmt "[%s]" (String.concat ", " (variables @ effects @ exns))

let rec ty_to_string =
  let tyclass_args_to_string args =
    String.concat " " (List.map ty_to_string args)
  in
  function
  | Ty x -> Ident.Type.to_string x
  | TyVar x -> Ident.TypeVar.to_string x
  | Eff effects -> eff_to_string effects
  | Fun (Ty x, eff, ret) when eff_is_empty eff ->
      fmt "%s -> %s" (Ident.Type.to_string x) (ty_to_string ret)
  | Fun (Ty x, eff, ret) ->
      fmt "%s -%s-> %s" (Ident.Type.to_string x) (eff_to_string eff) (ty_to_string ret)
  | Fun (x, eff, ret) ->
      fmt "(%s) -%s-> %s" (ty_to_string x) (eff_to_string eff) (ty_to_string ret)
  | Forall (x, k, t) ->
      fmt "forall %s : %s. %s" (Ident.TypeVar.to_string x) (Kinds.to_string k) (ty_to_string t)
  | TyClass ((name, _, args), eff, t) when eff_is_empty eff -> (* TODO: Display tyvars ? *)
      fmt "{%s %s} => %s" (Ident.TyClass.to_string name) (tyclass_args_to_string args) (ty_to_string t)
  | TyClass ((name, _, args), eff, t) -> (* TODO: Display tyvars ? *)
      fmt "{%s %s} =%s=> %s" (Ident.TyClass.to_string name) (tyclass_args_to_string args) (eff_to_string eff) (ty_to_string t)
  | AbsOnTy (name, k, t) ->
      fmt "λ%s : %s. %s" (Ident.TypeVar.to_string name) (Kinds.to_string k) (ty_to_string t)
  | AppOnTy (Ty f, Ty x) -> fmt "%s %s" (Ident.Type.to_string f) (Ident.Type.to_string x)
  | AppOnTy (Ty f, x) -> fmt "%s (%s)" (Ident.Type.to_string f) (ty_to_string x)
  | AppOnTy (f, Ty x) -> fmt "(%s) %s" (ty_to_string f) (Ident.Type.to_string x)
  | AppOnTy (f, x) -> fmt "(%s) (%s)" (ty_to_string f) (ty_to_string x)

let rec ty_reduce = function
  | AppOnTy (t, ty) ->
      begin match ty_reduce t with
      | AbsOnTy (name, _, t) -> ty_reduce (ty_replace ~from:name ~ty t)
      | Ty _ | TyVar _ | AppOnTy _ as t -> AppOnTy (t, ty_reduce ty)
      | Eff _ | Fun _ | Forall _ | TyClass _ -> assert false
      end
  | Ty _ | TyVar _ | Eff _ as ty -> ty
  | Fun (x, eff, y) -> Fun (ty_reduce x, eff, ty_reduce y)
  | Forall (x, k, t) -> Forall (x, k, ty_reduce t)
  | TyClass ((x, tyvars, args), eff, t) ->
      let args = List.map ty_reduce args in
      TyClass ((x, tyvars, args), eff, ty_reduce t)
  | AbsOnTy (name, k, ty) -> AbsOnTy (name, k, ty_reduce ty)

and ty_replace ~from ~ty =
  let rec aux = function
    | TyVar x when Ident.TypeVar.equal x from -> ty
    | TyVar _ | Ty _ as t -> t
    | Eff effects -> Eff (eff_replace ~from ~ty effects)
    | Fun (param, eff, ret) ->
        Fun (aux param, eff_replace ~from ~ty eff, aux ret)
    | Forall (x, k, t) -> Forall (x, k, aux t)
    | TyClass ((x, tyvars, args), eff, t) ->
        let args = List.map aux args in
        let eff = eff_replace ~from ~ty eff in
        TyClass ((x, tyvars, args), eff, aux t)
    | AbsOnTy (x, k, t) -> AbsOnTy (x, k, aux t)
    | AppOnTy (f, x) -> ty_reduce (AppOnTy (aux f, aux x))
  in
  aux

type tmp = t

module Instances = Utils.EqMap(struct
    type t = tmp list

    let equal = List.equal ty_equal
  end)

(* TODO: Handle contraints *)
type class_t =
  { params : (tyvar_name * Kinds.t) list
  ; signature : (name * t) list
  ; instances : name Instances.t
  }

let params_equal (name1, k1) (name2, k2) =
  (* TODO: Do not test typevar equality *)
  Ident.TypeVar.equal name1 name2 && Kinds.equal k1 k2

let signature_equal (name1, ty1) (name2, ty2) =
  Ident.Name.equal name1 name2
  && ty_equal ty1 ty2

let class_equal a b =
  List.equal params_equal a.params b.params
  && List.equal signature_equal a.signature b.signature
  && Instances.equal Ident.Name.equal a.instances b.instances
