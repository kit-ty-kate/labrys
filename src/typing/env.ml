(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

let empty = TypedEnv.{
  values = EnvMap.Value.empty;
  constrs = EnvMap.Constr.empty;
  types = EnvMap.Type.empty;
}

let ensure_unique _ _ _ = assert false

let union x y = TypedEnv.{
  values = EnvMap.Value.union ensure_unique x.values y.values;
  constrs = EnvMap.Constr.union ensure_unique x.constrs y.constrs;
  types = EnvMap.Type.union ensure_unique x.types y.types;
}

let diff x y = TypedEnv.{
  values = EnvMap.Value.diff x.values y.values;
  constrs = EnvMap.Constr.diff x.constrs y.constrs;
  types = EnvMap.Type.diff x.types y.types;
}

let constrs_to_values constrs =
  let aux k x = EnvMap.Value.add (Ident.Constr.to_name k) x in
  EnvMap.Constr.fold aux constrs EnvMap.Value.empty

let get_untyped_values env =
  let to_lident f k _ = LIdent.create (f k) in
  let constrs = env.TypedEnv.constrs in
  let constrs = EnvMap.Constr.mapi (to_lident Ident.Constr.to_string) constrs in
  let constrs = constrs_to_values constrs in
  let values = env.TypedEnv.values in
  let values = EnvMap.Value.mapi (to_lident Ident.Name.to_string) values in
  EnvMap.Value.union ensure_unique constrs values

let fail_not_provided thing name =
  Err.fail_module
    "The implementation does not match the interface: \
     The %s '%s' is required but not provided."
    thing
    name

let fail_value_mismatch name ~intf ~impl =
  Err.fail_doc
    ~loc:(Ident.Name.loc name)
    Utils.PPrint.(str "The implementation does not match the interface:" ^//^
                  str "The value has type" ^^^
                  squotes (NType.dump impl) ^^^
                  str "but the interface expects a value of type" ^^^
                  squotes (NType.dump intf) ^^
                  dot)

let fail_type_mismatch name ~intf ~impl =
  Err.fail_doc
    ~loc:(Ident.Type.loc name)
    Utils.PPrint.(str "The implementation does not match the interface:" ^//^
                  (str "The type has the form:" ^//^
                   NType.dump_aty name impl) ^/^
                  (str "but the interface expects a type of the form:" ^//^
                   NType.dump_aty name intf))

let fail_constr_mismatch name ~intf ~impl =
  Err.fail_doc
    ~loc:(Ident.Constr.loc name)
    Utils.PPrint.(str "The implementation does not match the interface:" ^//^
                  (str "The exception has the form:" ^//^
                   NType.dump_constr name impl) ^/^
                  (str "but the interface expects an exception of the form:" ^//^
                   NType.dump_constr name intf))

let vdiff_value env name ty = match EnvMap.Value.get name env with
  | None -> fail_not_provided "value" (Ident.Name.to_string name)
  | Some ty' when NType.is_subset_of ty' ty -> ()
  | Some ty' -> fail_value_mismatch name ~intf:ty ~impl:ty'

let vdiff_constr env name constr =
  let is_subset_of (constr1, ty1) (constr2, ty2) =
    NType.is_subset_of ty1 ty2 &&
    match constr1, constr2 with
    | TypedEnv.Exn, TypedEnv.Exn -> true
    | TypedEnv.Index idx1, TypedEnv.Index idx2 -> Int.equal idx1 idx2
    | TypedEnv.Exn, _ | TypedEnv.Index _, _ -> false
  in
  match EnvMap.Constr.get name env with
  | None -> fail_not_provided "data constructor" (Ident.Constr.to_string name)
  | Some constr' when is_subset_of constr' constr -> ()
  | Some constr' -> fail_constr_mismatch name ~intf:constr ~impl:constr'

let vdiff_type env name aty = match EnvMap.Type.get name env with
  | None -> fail_not_provided "type" (Ident.Type.to_string name)
  | Some aty' when NType.aty_is_subset_of aty' aty -> ()
  | Some aty' -> fail_type_mismatch name ~intf:aty ~impl:aty'

let check_vdiff TypedEnv.{values; constrs; types} y =
  EnvMap.Value.iter (vdiff_value y.TypedEnv.values) values;
  EnvMap.Type.iter (vdiff_type y.TypedEnv.types) types;
  EnvMap.Constr.iter (vdiff_constr y.TypedEnv.constrs) constrs

type add = TypedEnv.env -> TypedEnv.env

let add_value name ty env =
  if Ident.Name.equal name Builtins.underscore then
    env
  else
    let values = EnvMap.Value.add name ty env.TypedEnv.values in
    {env with TypedEnv.values}

let add_abstract_type name k env =
  let types = EnvMap.Type.add name (TypedEnv.Abstract k) env.TypedEnv.types in
  {env with TypedEnv.types}

let is_duplicated_constr name =
  List.exists (fun (x, _, _) -> Ident.Constr.equal name x)

let map_variants env =
  let aux variants idx (name, ty) =
    if is_duplicated_constr name variants then
      Err.fail
        ~loc:(Ident.Constr.loc name)
        "A data constructor with the same name already exists in this variant";
    variants @ [(name, idx, NType.check ~pure_arrow:`Partial env ty)]
  in
  List.foldi aux []

let add_datatype name k variants env =
  (* TODO: Check kind ? *)
  let variants =
    let env = add_abstract_type name k env in
    map_variants env variants
  in
  let constrs =
    List.fold_left
      (fun constrs (name, idx, ty) -> EnvMap.Constr.add name (TypedEnv.Index idx, ty) constrs)
      env.TypedEnv.constrs
      variants
  in
  let types = EnvMap.Type.add name (TypedEnv.Datatype (k, variants)) env.TypedEnv.types in
  {env with TypedEnv.constrs; TypedEnv.types}

let add_type_alias name ty env =
  let (ty, k) = Type.check ~pure_arrow:`Forbid env ty in
  let types = EnvMap.Type.add name (TypedEnv.Alias (k, ty)) env.TypedEnv.types in
  {env with TypedEnv.types}

let add_exception name ty env =
  let tname = Ident.Constr.to_type name in
  let targ = TypedEnv.Abstract TypedEnv.KExn in
  let types = EnvMap.Type.add tname targ env.TypedEnv.types in
  let env = {env with TypedEnv.types} in
  let ty = NType.check ~pure_arrow:`Partial env ty in
  let carg = (TypedEnv.Exn, ty) in
  let constrs = EnvMap.Constr.add name carg env.TypedEnv.constrs in
  {env with TypedEnv.constrs}
