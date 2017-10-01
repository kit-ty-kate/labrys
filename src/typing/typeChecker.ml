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
let is_main ~current_module = Ident.Name.equal (Builtins.main ~current_module)

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

let check_foreign_type options env ty =
  assert false (* TODO *)

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
      let ty = check_foreign_type options env ty in
      let acc = acc @ [Foreign (cname, name, ty)] in
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
  (); (* TODO: Check interface correspondance *)
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
