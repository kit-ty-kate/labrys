(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t =
  { values : PrivateTypes.t EnvMap.Value.t
  ; variants : (int * PrivateTypes.t * int) EnvMap.Variant.t
  ; types : PrivateTypes.visibility EnvMap.Types.t
  ; type_vars : PrivateTypes.kind EnvMap.TypeVar.t
  ; constructors : (Ident.TypeVar.t list * (PrivateTypes.t list * int) EnvMap.Index.t) EnvMap.Constr.t
  ; exceptions : PrivateTypes.t list EnvMap.Exn.t
  ; tyclasses : PrivateTypes.class_t EnvMap.TyClass.t
  ; named_instances : (Ident.TyClass.t * PrivateTypes.t list) EnvMap.Instance.t
  }

let empty =
  { values = EnvMap.Value.empty
  ; variants = EnvMap.Variant.empty
  ; types = EnvMap.Types.empty
  ; type_vars = EnvMap.TypeVar.empty
  ; constructors = EnvMap.Constr.empty
  ; exceptions = EnvMap.Exn.empty
  ; tyclasses = EnvMap.TyClass.empty
  ; named_instances = EnvMap.Instance.empty
  }

let add_value k x self = {self with values = EnvMap.Value.add k x self.values}
let add_variant k x self = {self with variants = EnvMap.Variant.add k x self.variants}
let add_type k x self = {self with types = EnvMap.Types.add k x self.types}
let add_type_var k x self = {self with type_vars = EnvMap.TypeVar.add k x self.type_vars}
let add_constr k k2 args x self = {self with constructors = EnvMap.Constr.add k k2 args x self.constructors}
let add_exception k x self = {self with exceptions = EnvMap.Exn.add k x self.exceptions}
let add_tyclass k x self = {self with tyclasses = EnvMap.TyClass.add k x self.tyclasses}
let add_named_instance k x self = {self with named_instances = EnvMap.Instance.add k x self.named_instances}

let replace_tyclass k x self = {self with tyclasses = EnvMap.TyClass.replace k x self.tyclasses}

let check_toplevel_type_vars type_vars =
  (* NOTE: Toplevel environment have to be free of type variables *)
  if not (EnvMap.TypeVar.is_empty type_vars) then
    assert false

let union a b =
  let values = EnvMap.Value.merge a.values b.values in
  let variants = EnvMap.Variant.merge a.variants b.variants in
  let types = EnvMap.Types.merge a.types b.types in
  let type_vars =
    check_toplevel_type_vars a.type_vars;
    check_toplevel_type_vars b.type_vars;
    EnvMap.TypeVar.empty
  in
  let constructors = EnvMap.Constr.merge a.constructors b.constructors in
  let exceptions = EnvMap.Exn.merge a.exceptions b.exceptions in
  let tyclasses = EnvMap.TyClass.merge a.tyclasses b.tyclasses in
  let named_instances =
    EnvMap.Instance.merge a.named_instances b.named_instances
  in
  {values; variants; types; type_vars; constructors; exceptions; tyclasses; named_instances}

let ty_equal x y = match x, y with
  | (PrivateTypes.Abstract k1 | PrivateTypes.Alias (_, k1)), PrivateTypes.Abstract k2
  | PrivateTypes.Abstract k1, PrivateTypes.Alias (_, k2) ->
      PrivateTypes.kind_equal k1 k2
  | PrivateTypes.Alias (ty1, k1), PrivateTypes.Alias (ty2, k2) ->
      PrivateTypes.ty_equal ty1 ty2 && PrivateTypes.kind_equal k1 k2

let variant_equal (idx1, ty1, len1) (idx2, ty2, len2) =
  PrivateTypes.ty_equal ty1 ty2 && Int.equal idx1 idx2 && Int.equal len1 len2

let idx_equal (x, y) (x', y') = List.equal PrivateTypes.ty_equal x x' && Int.equal y y'

let constr_equal (x, y) (x', y') =
  List.equal Ident.TypeVar.equal x x' && EnvMap.Index.equal idx_equal y y'

let named_instances_equal (name1, args1) (name2, args2) =
  (* TODO: Allow forall a. Class a ? *)
  let map_args = List.map (fun arg -> ([], arg)) in
  Ident.TyClass.equal name1 name2
  && PrivateTypes.tyclass_args_equal (map_args args1) (map_args args2)

let is_subset_of a b =
  check_toplevel_type_vars a.type_vars;
  check_toplevel_type_vars b.type_vars;
  EnvMap.Value.diff ~eq:PrivateTypes.ty_equal a.values b.values
  @ EnvMap.Variant.diff ~eq:variant_equal a.variants b.variants
  @ EnvMap.Types.diff ~eq:ty_equal a.types b.types
  @ EnvMap.Constr.diff ~eq:constr_equal a.constructors b.constructors
  @ EnvMap.Exn.diff ~eq:(List.equal PrivateTypes.ty_equal) a.exceptions b.exceptions
  @ EnvMap.TyClass.diff ~eq:PrivateTypes.class_equal a.tyclasses b.tyclasses
  @ EnvMap.Instance.diff ~eq:named_instances_equal a.named_instances b.named_instances

let get_untyped_values self =
  let env = EnvMap.Value.empty in
  let add k _ env =
    let to_string = Ident.Name.to_string in
    EnvMap.Value.add k (LIdent.create (to_string k)) env
  in
  let env = EnvMap.Value.fold add self.values env in
  let add k _ env =
    let to_name = Ident.Variant.to_name in
    let to_string = Ident.Variant.to_string in
    EnvMap.Value.add (to_name k) (LIdent.create (to_string k)) env
  in
  let env = EnvMap.Variant.fold add self.variants env in
  env
