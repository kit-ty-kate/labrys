(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

module Exn_set = PrivateTypes.Exn_set
module Variables = PrivateTypes.Variables
module Effects = PrivateTypes.Effects

type t = PrivateTypes.effects =
  { variables : Variables.t
  ; effects : Effects.t
  ; exns : Exn_set.t
  }

let empty =
  { variables = Variables.empty
  ; effects = Effects.empty
  ; exns = Exn_set.empty
  }

let is_empty = PrivateTypes.eff_is_empty

let equal = PrivateTypes.eff_equal
let is_subset_of = PrivateTypes.eff_is_subset_of

let has_io options {variables; effects; exns = _} =
  not (Variables.is_empty variables)
  || Effects.mem (Builtins.io options) effects

let union_ty = PrivateTypes.eff_union_ty'
let union = PrivateTypes.eff_union

let add options gamma eff self =
  match eff with
  | DesugaredTree.EffTy (name, exns) ->
      let (k, has_args, self) =
        match GammaMap.Types.find name gamma.Gamma.types with
        | PrivateTypes.Abstract k ->
            if Ident.Type.equal name (Builtins.exn options) then
              (k, true, self)
            else
              (k, false, {self with effects = Effects.add name self.effects})
        | (PrivateTypes.Alias (ty, k)) ->
            (k, false, union_ty self ty)
      in
      if not (Kinds.is_effect k) then
        Err.fail
          ~loc:(Ident.Type.loc name)
          "Only kind φ is accepted here";
      if has_args && List.is_empty exns then
        Err.fail
          ~loc:(Ident.Type.loc name)
          "The '%s' effect must have at least one argument"
          (Ident.Type.to_string name);
      if not has_args && not (List.is_empty exns) then
        Err.fail
          ~loc:(Ident.Type.loc name)
          "The '%s' effect doesn't have any arguments"
          (Ident.Type.to_string name);
      let exns = Exn_set.of_list exns in
      let exns = Exn_set.union exns self.exns in
      {self with exns}
  | DesugaredTree.EffTyVar name ->
      let k = GammaMap.TypeVar.find name gamma.Gamma.type_vars in
      if not (Kinds.is_effect k) then
        Err.fail
          ~loc:(Ident.TypeVar.loc name)
          "Only kind φ is accepted here";
      let variables = Variables.add name self.variables in
      {self with variables}

let add_exn x self =
  {self with exns = Exn_set.add x self.exns}

let union3 x y z =
  union (union x y) z

let union5 a b c d e =
  union (union (union (union a b) c) d) e

let remove_exn x self =
  if not (Exn_set.mem x self.exns) then
    Err.fail
      ~loc:(Ident.Exn.loc x)
      "Useless case. The exception '%s' is not included in the handled \
       expression"
      (Ident.Exn.to_string x);
  let exns = Exn_set.remove x self.exns in
  {self with exns}

let to_string = PrivateTypes.eff_to_string

let of_list options gamma (_, l) =
  let aux acc ty = add options gamma ty acc in
  List.fold_left aux empty l

let replace = PrivateTypes.eff_replace

let remove_tyclass_variables ~is_tyclass eff =
  let variables =
    let aux name eff =
      if is_tyclass name then
        Variables.remove name eff
      else
        eff
    in
    Variables.fold aux eff.variables eff.variables
  in
  {eff with variables}

let match_tyclass ~is_tyclass ~is_tyclass_x eff ~eff_x =
  let eff' = remove_tyclass_variables ~is_tyclass eff in
  let eff_x' = remove_tyclass_variables ~is_tyclass:is_tyclass_x eff_x in
  let aux is_tyclass eff eff' =
    let aux name acc =
      if is_tyclass name then
        (name, PrivateTypes.Eff eff') :: acc
      else
        acc
    in
    Variables.fold aux eff.variables []
  in
  let matched = aux is_tyclass eff eff_x' in
  let matched_x = aux is_tyclass_x eff_x eff' in
  let eff_union = union eff' eff_x' in
  match List.is_empty matched, List.is_empty matched_x with
  | true, true -> ([], eff', [], eff_x')
  | false, true -> (matched, eff_union, [], eff_x')
  | true, false -> ([], eff', matched_x, eff_union)
  | false, false -> (matched, eff_union, matched_x, eff_union)

let unify_tyclass ~is_new_tyvar eff ~eff_x =
  let aux name acc =
    if is_new_tyvar name then
      (name, PrivateTypes.Eff eff_x) :: acc
    else
      acc
  in
  Variables.fold aux eff.variables []

let contains_free_tyvars gammaTV self =
  let aux name = GammaSet.TypeVar.mem name gammaTV in
  Variables.for_all aux self.variables
