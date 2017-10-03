(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open UntypedTree

let type_fail ~loc ~has ~expected =
  Err.fail_doc ~loc
    Utils.PPrint.(str "This value has type" ^^^
                  squotes (Type.dump has) ^^^
                  str "but was expected of type" ^^^
                  squotes (Type.dump expected) ^^^
                  dot)

let unit options = TypedEnv.Ty (Builtins.unit options)
let is_unit options = Type.is_subset_of (unit options)
let is_main ~current_module = Ident.Name.equal (Builtins.main ~current_module)
let io options = TypedEnv.Ty (Builtins.io options)
let has_io options = List.exists (Type.is_subset_of (io options))

let check_term env t =
  assert false (* TODO *)

let check_eff_value ~current_module options name ty eff =
  let loc = Ident.Name.loc name in
  if options#with_main && is_main ~current_module name then begin
    if not (List.for_all (Ident.Type.equal (Builtins.io options)) eff) then
      Err.fail ~loc
        "Effects different than 'IO' are not allowed in the main function";
    if not (Type.is_subset_of ty (unit options)) then
      Err.fail_doc ~loc
        Utils.PPrint.(str "The main function is supposed to have type 'unit' \
                           but got type" ^^^
                      squotes (Type.dump ty) ^^^
                      dot);
    true
  end else begin
    if not (List.is_empty eff) then
      Err.fail ~loc "Effects are not allowed on toplevel";
    false
  end

let rec get_foreign_type ~default options = function
  | TypedEnv.TAlias (_, ty) | TypedEnv.Forall (_, _, ty) ->
      get_foreign_type ~default options ty
  | TypedEnv.Ty name ->
      let arg_ty_map =
        [ (Builtins.int options, `Int ())
        ; (Builtins.float options, `Float ())
        ; (Builtins.char options, `Char ())
          (* NOTE: String is not present because it is a pointer *)
        ]
      in
      Option.get_or ~default
        (List.Assoc.get ~eq:Ident.Type.equal name arg_ty_map)
  | TypedEnv.App _ | TypedEnv.Fun _ ->
      default
  | TypedEnv.Eff _ | TypedEnv.Abs _ ->
      assert false (* NOTE: This shouldn't happen *)

let rec check_foreign_type ~loc is_first options env = function
  | TypedEnv.TAlias (_, ty) | TypedEnv.Forall (_, _, ty) ->
      check_foreign_type ~loc true options env ty
  | TypedEnv.Ty _ | TypedEnv.App _ ->
      Err.fail ~loc "Cannot bind a global variable"
  | TypedEnv.Eff _ | TypedEnv.Abs _ ->
      assert false (* NOTE: This shouldn't happen *)
  | TypedEnv.Fun (t1, e, TypedEnv.TAlias (_, t2)) ->
      check_foreign_type ~loc true options env (TypedEnv.Fun (t1, e, t2))
  | TypedEnv.Fun (t1, e, (TypedEnv.Ty _ | TypedEnv.App _ as t2)) ->
      if not (has_io options e) then
        Err.fail ~loc "Bindings cannot be pure. All bindings have \
                       to use the IO effect on the final arrow";
      let t1 =
        if is_first && is_unit options t1
        then []
        else [get_foreign_type ~default:`Custom options t1]
      in
      let t2 = get_foreign_type ~default:`Void options t2 in
      (t2, t1)
  | TypedEnv.Fun (t1, _, t2) ->
      let t1 = get_foreign_type ~default:`Custom options t1 in
      let (ret, t2) = check_foreign_type ~loc false options env t2 in
      (ret, [t1] @ t2)

let check_top ~current_module options (acc, has_main, env) = function
  | PretypedTree.Value (name, t) ->
      let (t, ty, eff) = check_term env t in
      let has_main = check_eff_value ~current_module options name ty eff in
      let acc = acc @ [Value (name, t)] in
      let env = Env.add_toplevel_value name ty env in
      (acc, has_main, env)
  | PretypedTree.Type (name, ty) ->
      let env = Env.add_type_alias name ty env in
      (acc, has_main, env)
  | PretypedTree.Foreign (cname, name, ty) ->
      let ty = Type.check_value ~pure_arrow:`Partial env ty in
      let rty =
        check_foreign_type ~loc:(Ident.Name.loc name) true options env ty
      in
      let acc = acc @ [Foreign (cname, name, rty)] in
      let env = Env.add_toplevel_value name ty env in
      (acc, has_main, env)
  | PretypedTree.Datatype (name, k, variants) ->
      let env = Env.add_datatype name k variants env in
      (acc, has_main, env)
  | PretypedTree.Exception (name, args) ->
      let acc = acc @ [Exception name] in
      let env = Env.add_exception name args env in
      (acc, has_main, env)
  | PretypedTree.Class _ ->
        assert false (* TODO *)
  | PretypedTree.Instance _ ->
        assert false (* TODO *)

let check ~current_module ~interface options env x =
  let check_top = check_top ~current_module options in
  let (res, has_main, env) = List.fold_left check_top ([], false, env) x in
  if options#with_main && not has_main then
    Err.fail_module "No 'main' value found in the main module";
  Env.check_vdiff interface env;
  res

let check_interface ~current_module options =
  let aux env = function
    | PretypedTree.IVal (name, ty) ->
        let ty = Type.check_value ~pure_arrow:`Partial env ty in
        Env.add_toplevel_value name ty env
    | PretypedTree.IAbstractType (name, k) ->
        Env.add_abstract_type name k env
    | PretypedTree.IDatatype (name, k, variants) ->
        Env.add_datatype name k variants env
    | PretypedTree.ITypeAlias (name, ty) ->
        Env.add_type_alias name ty env
    | PretypedTree.IException (name, args) ->
        Env.add_exception name args env
    | PretypedTree.IClass _ ->
        assert false (* TODO *)
    | PretypedTree.IInstance _ ->
        assert false (* TODO *)
  in
  fun env l ->
    Env.diff (List.fold_left aux env l) env
