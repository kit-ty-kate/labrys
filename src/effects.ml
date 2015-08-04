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

let fmt = Printf.sprintf

module Exn_set = PrivateTypes.Exn_set
module Variables = PrivateTypes.Variables

type t = PrivateTypes.effects =
  { variables : Variables.t
  ; exns : Exn_set.t
  }

let empty =
  { variables = Variables.empty
  ; exns = Exn_set.empty
  }

let is_empty {variables; exns} =
  Variables.is_empty variables
  && Exn_set.is_empty exns

let equal = PrivateTypes.eff_equal
let is_subset_of = PrivateTypes.eff_is_subset_of

let has_io {variables; exns = _} =
  not (Variables.is_empty variables)

let rec add options gamma (name, exns) self =
  let exns =
    let aux x = fst (GammaMap.Exn.find_binding x gamma.Gamma.exceptions) in
    List.map aux exns
  in
  let (name, k, self) =
    match GammaMap.Types.find_binding name gamma.Gamma.types with
    | (name, PrivateTypes.Abstract k) -> (name, k, self)
    | (name, PrivateTypes.Alias (ty, k)) -> (name, k, union_ty self ty)
  in
  if not (Kinds.is_effect k) then
    Err.fail
      ~loc:(Ident.Type.loc name)
      "Only kind φ is accepted here";
  let has_args = Ident.Type.equal name (Builtins.exn options) in
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
  let variables =
    if has_args then
      self.variables
    else
      Variables.add name self.variables
  in
  {variables; exns}

and union_ty ?from self = function
  | PrivateTypes.Ty name ->
      let variables = match from with
        | Some from -> Variables.remove from self.variables
        | None -> self.variables
      in
      let variables = Variables.add name variables in
      {self with variables}
  | PrivateTypes.Eff eff ->
      union self eff
  | PrivateTypes.Fun _
  | PrivateTypes.Forall _
  | PrivateTypes.TyClass _
  | PrivateTypes.AbsOnTy _
  | PrivateTypes.AppOnTy _ -> assert false

and union x y =
  let variables = Variables.union x.variables y.variables in
  let exns = Exn_set.union x.exns y.exns in
  {variables; exns}

let add_exn x self =
  {self with exns = Exn_set.add x self.exns}

let union3 x y z =
  union (union x y) z

let remove_exn x self =
  if not (Exn_set.mem x self.exns) then
    Err.fail
      ~loc:(Ident.Exn.loc x)
      "Useless case. The exception '%s' is not included in the handled \
       expression"
      (Ident.Exn.to_string x);
  let exns = Exn_set.remove x self.exns in
  {self with exns}

let to_string self =
  let aux name acc = Ident.Type.to_string name :: acc in
  let exns =
    if Exn_set.is_empty self.exns then
      []
    else
      let aux name acc = Ident.Exn.to_string name :: acc in
      [fmt "Exn [%s]" (String.concat " | " (Exn_set.fold aux self.exns []))]
  in
  fmt "[%s]" (String.concat ", " (exns @ Variables.fold aux self.variables []))

let of_list options gamma (_, l) =
  let aux acc ty = add options gamma ty acc in
  List.fold_left aux empty l

let replace ~from ~ty self =
  let aux name self =
    if Ident.Type.equal name from then
      union_ty ~from self ty
    else
      {self with variables = Variables.add name self.variables}
  in
  Variables.fold aux self.variables empty

let remove_module_aliases = PrivateTypes.eff_remove_module_aliases
