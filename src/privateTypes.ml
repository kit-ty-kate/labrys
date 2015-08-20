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

open Monomorphic_containers.Open

module Exn_set = Utils.EqSet(Ident.Exn)
module Variables = Utils.EqSet(Ident.Type)

type name = Ident.Name.t
type ty_name = Ident.Type.t

type effects =
  { variables : Variables.t
  ; exns : Exn_set.t
  }

type tyclass_arg =
  | Param of (ty_name * Kinds.t)
  | Filled of t

and t =
  | Ty of ty_name
  | Eff of effects
  | Fun of (t * effects * t)
  | Forall of (ty_name * Kinds.t * t)
  | TyClass of ((Ident.TyClass.t * tyclass_arg list) * effects * t)
  | AbsOnTy of (ty_name * Kinds.t * t)
  | AppOnTy of (t * t)

type visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

let fmt = Printf.sprintf

let ty_empty options =
  let aux gammaT eff = GammaMap.Types.add eff (Abstract Kinds.Eff) gammaT in
  List.fold_left aux GammaMap.Types.empty (Builtins.effects options)

let var_eq list_eq x y =
  let aux k =
    let mem k = Variables.mem k y in
    match List.find_pred (fun (k', _) -> Ident.Type.equal k k') list_eq with
    | Some (_, k) -> mem k
    | None -> mem k
  in
  Variables.for_all aux x

let eff_equal list_eq x y =
  Int.(Variables.cardinal x.variables = Variables.cardinal y.variables)
  && var_eq list_eq x.variables y.variables
  && Exn_set.equal x.exns y.exns

let eff_is_subset_of list_eq x y =
  var_eq list_eq x.variables y.variables
  && Exn_set.subset x.exns y.exns

let eff_empty =
  { variables = Variables.empty
  ; exns = Exn_set.empty
  }

let eff_union x y =
  let variables = Variables.union x.variables y.variables in
  let exns = Exn_set.union x.exns y.exns in
  {variables; exns}

let eff_union_ty ?from self = function
  | Ty name ->
      let variables = match from with
        | Some from -> Variables.remove from self.variables
        | None -> self.variables
      in
      let variables = Variables.add name variables in
      {self with variables}
  | Eff eff ->
      eff_union self eff
  | Fun _
  | Forall _
  | TyClass _
  | AbsOnTy _
  | AppOnTy _ -> assert false

let eff_replace ~from ~ty self =
  let aux name self =
    if Ident.Type.equal name from then
      eff_union_ty ~from self ty
    else
      {self with variables = Variables.add name self.variables}
  in
  Variables.fold aux self.variables eff_empty

let ty_equal eff_eq x y =
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
    | TyClass ((name1, args1), eff1, t), TyClass ((name2, args2), eff2, t') ->
        begin try
          let l =
            let aux acc x y = match x, y with
              | Param (param1, _), Param (param2, _) ->
                  (param1, param2) :: acc
              | Filled ty1, Filled ty2 ->
                  if not (aux eq_list (ty1, ty2)) then
                    raise Not_found;
                  acc
              | Param _, _ | Filled _, _ ->
                  raise Not_found
            in
            List.fold_left2 aux [] args1 args2
          in
          (* NOTE: no need to check kinds as it is already checked *)
          Ident.TyClass.equal name1 name2
          && eff_eq eq_list eff1 eff2
          && aux (l @ eq_list) (t, t')
        with Not_found ->
          false
        end
    | AppOnTy _, _
    | AbsOnTy _, _
    | Forall _, _
    | TyClass _, _
    | Ty _, _
    | Eff _, _
    | Fun _, _ -> false
  in
  aux [] (x, y)

let ty_is_subset_of = ty_equal eff_is_subset_of

let ty_equal = ty_equal eff_equal

let tyclass_args_equal args1 args2 =
  let aux arg1 arg2 = match arg1, arg2 with
    | Filled _, Param _ | Param _, Param _ -> true
    | Filled ty1, Filled ty2 -> ty_equal ty1 ty2
    | Param _, _ -> false
  in
  try List.for_all2 aux args1 args2 with Invalid_argument _ -> assert false

let eff_remove_module_aliases vars {variables; exns} =
  let variables =
    let aux name =
      if not (List.mem name vars) then
        Ident.Type.remove_aliases name
      else
        name
    in
    Variables.map aux variables
  in
  let exns = Exn_set.map Ident.Exn.remove_aliases exns in
  {variables; exns}

let eff_is_empty {variables; exns} =
  Variables.is_empty variables
  && Exn_set.is_empty exns

let eff_to_string self =
  let aux name acc = Ident.Type.to_string name :: acc in
  let exns =
    if Exn_set.is_empty self.exns then
      []
    else
      let aux name acc = Ident.Exn.to_string name :: acc in
      [fmt "Exn [%s]" (String.concat " | " (Exn_set.fold aux self.exns []))]
  in
  fmt "[%s]" (String.concat ", " (exns @ Variables.fold aux self.variables []))

let rec tyclass_arg_remove_module_aliases vars = function
  | Param _ as arg -> arg
  | Filled ty -> Filled (ty_remove_module_aliases vars ty)

and ty_remove_module_aliases vars = function
  | Ty name when not (List.mem name vars) ->
      Ty (Ident.Type.remove_aliases name)
  | Ty name ->
      Ty name
  | Eff effects ->
      Eff (eff_remove_module_aliases vars effects)
  | Fun (x, eff, y) ->
      Fun (ty_remove_module_aliases vars x, eff_remove_module_aliases vars eff, ty_remove_module_aliases vars y)
  | Forall (name, k, t) ->
      Forall (name, k, ty_remove_module_aliases (name :: vars) t)
  | TyClass ((name, args), eff, t) ->
      let args = List.map (tyclass_arg_remove_module_aliases vars) args in
      let var =
        let aux acc = function
          | Param (x, _) -> x :: acc
          | Filled _ -> acc
        in
        List.fold_left aux [] args
      in
      let eff = eff_remove_module_aliases vars eff in
      TyClass ((name, args), eff, ty_remove_module_aliases (var @ vars) t)
  | AbsOnTy (name, k, t) ->
      AbsOnTy (name, k, ty_remove_module_aliases (name :: vars) t)
  | AppOnTy (x, y) ->
      AppOnTy (ty_remove_module_aliases vars x, ty_remove_module_aliases vars y)

let tyclass_args_remove_module_aliases = List.map (tyclass_arg_remove_module_aliases [])
let ty_remove_module_aliases = ty_remove_module_aliases []

let rec ty_to_string =
  let tyclass_arg_to_string = function
    | Param (x, _) -> Ident.Type.to_string x
    | Filled ty -> fmt "[%s]" (ty_to_string ty)
  in
  let tyclass_args_to_string args =
    String.concat " " (List.map tyclass_arg_to_string args)
  in
  function
  | Ty x -> Ident.Type.to_string x
  | Eff effects -> eff_to_string effects
  | Fun (Ty x, eff, ret) when eff_is_empty eff ->
      fmt "%s -> %s" (Ident.Type.to_string x) (ty_to_string ret)
  | Fun (Ty x, eff, ret) ->
      fmt "%s -%s-> %s" (Ident.Type.to_string x) (eff_to_string eff) (ty_to_string ret)
  | Fun (x, eff, ret) ->
      fmt "(%s) -%s-> %s" (ty_to_string x) (eff_to_string eff) (ty_to_string ret)
  | Forall (x, k, t) ->
      fmt "forall %s : %s. %s" (Ident.Type.to_string x) (Kinds.to_string k) (ty_to_string t)
  | TyClass ((name, args), eff, t) when eff_is_empty eff ->
      fmt "{%s %s} => %s" (Ident.TyClass.to_string name) (tyclass_args_to_string args) (ty_to_string t)
  | TyClass ((name, args), eff, t) ->
      fmt "{%s %s} =%s=> %s" (Ident.TyClass.to_string name) (tyclass_args_to_string args) (eff_to_string eff) (ty_to_string t)
  | AbsOnTy (name, k, t) ->
      fmt "λ%s : %s. %s" (Ident.Type.to_string name) (Kinds.to_string k) (ty_to_string t)
  | AppOnTy (Ty f, Ty x) -> fmt "%s %s" (Ident.Type.to_string f) (Ident.Type.to_string x)
  | AppOnTy (Ty f, x) -> fmt "%s (%s)" (Ident.Type.to_string f) (ty_to_string x)
  | AppOnTy (f, Ty x) -> fmt "(%s) %s" (ty_to_string f) (Ident.Type.to_string x)
  | AppOnTy (f, x) -> fmt "(%s) (%s)" (ty_to_string f) (ty_to_string x)

let rec ty_reduce = function
  | AppOnTy (t, ty) ->
      begin match ty_reduce t with
      | AbsOnTy (name, _, t) -> ty_reduce (ty_replace ~from:name ~ty t)
      | Ty _ | AppOnTy _ as t -> AppOnTy (t, ty)
      | Eff _ | Fun _ | Forall _ | TyClass _ -> assert false
      end
  | Ty _ | Eff _ | Fun _ | Forall _ | TyClass _ | AbsOnTy _ as ty -> ty

and ty_replace ~from ~ty =
  let rec aux = function
    | Ty x when Ident.Type.equal x from -> ty
    | Ty _ as t -> t
    | Eff effects -> Eff (eff_replace ~from ~ty effects)
    | Fun (param, eff, ret) ->
        Fun (aux param, eff_replace ~from ~ty eff, aux ret)
    | Forall (x, k, t) -> Forall (x, k, aux t)
    | TyClass ((x, args), eff, t) ->
        let args =
          let aux = function
            | Param _ as arg -> arg
            | Filled ty -> Filled (aux ty)
          in
          List.map aux args
        in
        let eff = eff_replace ~from ~ty eff in
        TyClass ((x, args), eff, aux t)
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
  { params : (ty_name * Kinds.t) list
  ; signature : (name * t) list
  ; instances : name Instances.t
  }

let class_remove_module_aliases {params; signature; instances} =
  let params =
    let aux (name, k) = (Ident.Type.remove_aliases name, k) in
    List.map aux params
  in
  let signature =
    let aux (name, ty) =
      (Ident.Name.remove_aliases name, ty_remove_module_aliases ty)
    in
    List.map aux signature
  in
  let instances =
    let aux = List.map ty_remove_module_aliases in
    Instances.map_keys aux instances
  in
  {params; signature; instances}

let params_equal (name1, k1) (name2, k2) =
  Ident.Type.equal name1 name2 && Kinds.equal k1 k2

let signature_equal (name1, ty1) (name2, ty2) =
  Ident.Name.equal name1 name2
  && ty_equal ty1 ty2

let class_equal a b =
  List.equal params_equal a.params b.params
  && List.equal signature_equal a.signature b.signature
  && Instances.equal Ident.Name.equal a.instances b.instances
