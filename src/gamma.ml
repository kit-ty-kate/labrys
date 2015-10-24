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

type t =
  { values : PrivateTypes.t GammaMap.Value.t
  ; variants : (int * PrivateTypes.t * int) GammaMap.Variant.t
  ; types : PrivateTypes.visibility GammaMap.Types.t
  ; type_vars : Kinds.t GammaMap.TypeVar.t
  ; constructors : (Ident.TypeVar.t list * (PrivateTypes.t list * int) GammaMap.Index.t) GammaMap.Constr.t
  ; exceptions : PrivateTypes.t list GammaMap.Exn.t
  ; tyclasses : PrivateTypes.class_t GammaMap.TyClass.t
  ; named_instances : (Ident.TyClass.t * PrivateTypes.t list) GammaMap.Instance.t
  }

let empty options =
  { values = GammaMap.Value.empty
  ; variants = GammaMap.Variant.empty
  ; types = PrivateTypes.ty_empty options
  ; type_vars = GammaMap.TypeVar.empty
  ; constructors = GammaMap.Constr.empty
  ; exceptions = GammaMap.Exn.empty
  ; tyclasses = GammaMap.TyClass.empty
  ; named_instances = GammaMap.Instance.empty
  }

let add_value k x self = {self with values = GammaMap.Value.add k x self.values}
let add_variant k x self = {self with variants = GammaMap.Variant.add k x self.variants}
let add_type k x self = {self with types = GammaMap.Types.add k x self.types}
let add_type_var k x self = {self with type_vars = GammaMap.TypeVar.add k x self.type_vars}
let add_constr k k2 args x self = {self with constructors = GammaMap.Constr.add k k2 args x self.constructors}
let add_exception k x self = {self with exceptions = GammaMap.Exn.add k x self.exceptions}
let add_tyclass k x self = {self with tyclasses = GammaMap.TyClass.add k x self.tyclasses}
let add_named_instance k x self = {self with named_instances = GammaMap.Instance.add k x self.named_instances}

let replace_tyclass k x self = {self with tyclasses = GammaMap.TyClass.replace k x self.tyclasses}

let check_toplevel_type_vars type_vars =
  (* NOTE: Toplevel environment have to be free of type variables *)
  if not (GammaMap.TypeVar.is_empty type_vars) then
    assert false

let union ~imported b =
  let values =
    let aux = PrivateTypes.ty_remove_module_aliases in
    GammaMap.Value.union aux ~imported:imported.values b.values
  in
  let variants =
    let aux (idx, ty, len) = (idx, PrivateTypes.ty_remove_module_aliases ty, len) in
    GammaMap.Variant.union aux ~imported:imported.variants b.variants
  in
  let types =
    let aux = function
      | PrivateTypes.Alias (ty, k) -> PrivateTypes.Alias (PrivateTypes.ty_remove_module_aliases ty, k)
      | PrivateTypes.Abstract _ as x -> x
    in
    GammaMap.Types.union aux ~imported:imported.types b.types
  in
  let type_vars =
    check_toplevel_type_vars imported.type_vars;
    check_toplevel_type_vars b.type_vars;
    GammaMap.TypeVar.empty
  in
  let constructors =
    let aux (args, idx) =
      let aux (tys, idx) =
        (List.map PrivateTypes.ty_remove_module_aliases tys, idx)
      in
      (args, GammaMap.Index.union aux ~imported:idx GammaMap.Index.empty)
    in
    GammaMap.Constr.union aux ~imported:imported.constructors b.constructors
  in
  let exceptions =
    let aux = List.map PrivateTypes.ty_remove_module_aliases in
    GammaMap.Exn.union aux ~imported:imported.exceptions b.exceptions
  in
  let tyclasses =
    let aux = PrivateTypes.class_remove_module_aliases in
    GammaMap.TyClass.union aux ~imported:imported.tyclasses b.tyclasses
  in
  let named_instances =
    let aux (name, tys) =
      let name = Ident.TyClass.remove_aliases name in
      let tys = PrivateTypes.tyclass_args_remove_module_aliases tys in
      (name, tys)
    in
    GammaMap.Instance.union aux ~imported:imported.named_instances b.named_instances
  in
  {values; variants; types; type_vars; constructors; exceptions; tyclasses; named_instances}

let ty_equal x y = match x, y with
  | (PrivateTypes.Abstract k1 | PrivateTypes.Alias (_, k1)), PrivateTypes.Abstract k2
  | PrivateTypes.Abstract k1, PrivateTypes.Alias (_, k2) ->
      Kinds.equal k1 k2
  | PrivateTypes.Alias (ty1, k1), PrivateTypes.Alias (ty2, k2) ->
      PrivateTypes.ty_equal ty1 ty2 && Kinds.equal k1 k2

let variant_equal (idx1, ty1, len1) (idx2, ty2, len2) =
  PrivateTypes.ty_equal ty1 ty2 && Int.equal idx1 idx2 && Int.equal len1 len2

let idx_equal (x, y) (x', y') = List.equal PrivateTypes.ty_equal x x' && Int.equal y y'

let constr_equal (x, y) (x', y') =
  List.equal Ident.TypeVar.equal x x' && GammaMap.Index.equal idx_equal y y'

let named_instances_equal (name1, args1) (name2, args2) =
  (* TODO: Allow forall a. Class a ? *)
  let map_args = List.map (fun arg -> ([], arg)) in
  Ident.TyClass.equal name1 name2
  && PrivateTypes.tyclass_args_equal (map_args args1) (map_args args2)

let is_subset_of a b =
  check_toplevel_type_vars a.type_vars;
  check_toplevel_type_vars b.type_vars;
  GammaMap.Value.diff ~eq:PrivateTypes.ty_equal a.values b.values
  @ GammaMap.Variant.diff ~eq:variant_equal a.variants b.variants
  @ GammaMap.Types.diff ~eq:ty_equal a.types b.types
  @ GammaMap.Constr.diff ~eq:constr_equal a.constructors b.constructors
  @ GammaMap.Exn.diff ~eq:(List.equal PrivateTypes.ty_equal) a.exceptions b.exceptions
  @ GammaMap.TyClass.diff ~eq:PrivateTypes.class_equal a.tyclasses b.tyclasses
  @ GammaMap.Instance.diff ~eq:named_instances_equal a.named_instances b.named_instances

let open_module modul {values; variants; types; type_vars; constructors; exceptions; tyclasses; named_instances} =
  let values = GammaMap.Value.open_module modul values in
  let variants = GammaMap.Variant.open_module modul variants in
  let types = GammaMap.Types.open_module modul types in
  let constructors = GammaMap.Constr.open_module modul constructors in
  let exceptions = GammaMap.Exn.open_module modul exceptions in
  let tyclasses = GammaMap.TyClass.open_module modul tyclasses in
  let named_instances = GammaMap.Instance.open_module modul named_instances in
  {values; variants; types; type_vars; constructors; exceptions; tyclasses; named_instances}
