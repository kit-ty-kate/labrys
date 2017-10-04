(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open UntypedTree

let type_fail ~loc ~has ~expected =
  Err.fail_doc ~loc
    Utils.PPrint.(str "This value has type" ^^^
                  squotes (NType.dump has) ^^^
                  str "but was expected of type" ^^^
                  squotes (NType.dump expected) ^^^
                  dot)

let eff_fail ~loc ~has ~expected =
  Err.fail_doc ~loc
    Utils.PPrint.(str "This effect has type" ^^^
                  squotes (NType.dump_eff has) ^^^
                  str "but was expected of type" ^^^
                  squotes (NType.dump_eff expected) ^^^
                  dot)

let check_type ~loc env ty' ty =
  let ty = NType.check ~pure_arrow:`Allow env ty in
  if not (NType.is_subset_of ty' ty) then
    type_fail ~loc ~has:ty' ~expected:ty;
  ty

let check_eff ~loc env eff' eff =
  let eff = NType.check_eff ~pure_arrow:`Allow env eff in
  if not (NType.eff_is_subset_of eff' eff) then
    eff_fail ~loc ~has:eff' ~expected:eff;
  eff

let check_eff_opt ~loc env eff' =
  Option.map_or ~default:[] (check_eff ~loc env eff')

let unit options = TypedEnv.NTy (Builtins.unit options)
let is_unit options = NType.is_subset_of (unit options)
let is_main ~current_module = Ident.Name.equal (Builtins.main ~current_module)
let io options = TypedEnv.NTy (Builtins.io options)
let has_io options = List.exists (NType.is_subset_of (io options))

let get_const options = function
  | PretypedTree.Int n -> (`Int n, Builtins.int options)
  | PretypedTree.Float n -> (`Float n, Builtins.float options)
  | PretypedTree.Char c -> (`Char c, Builtins.char options)
  | PretypedTree.String s -> (`String s, Builtins.string options)

let rec check_term options env = function
  | (loc, PretypedTree.Abs ((name, ty), t)) ->
      assert false (* TODO *)
  | (loc, PretypedTree.TAbs ((name, k), t)) ->
      assert false (* TODO *)
  | (_, PretypedTree.CAbs _) ->
      assert false (* TODO *)
  | (loc, PretypedTree.App (t1, t2)) ->
      assert false (* TODO *)
  | (loc, PretypedTree.TApp (t, ty)) ->
      assert false (* TODO *)
  | (_, PretypedTree.CApp _) ->
      assert false (* TODO *)
  | (_, PretypedTree.Val name) ->
      let ty = EnvMap.Value.find name env.TypedEnv.values in
      (Val name, ty, [])
  | (loc, PretypedTree.Var name) ->
      let (idx, ty) = EnvMap.Constr.find name env.TypedEnv.constrs in
      let size = NType.size ty in
      (Var (idx, size), ty, [])
  | (loc, PretypedTree.PatternMatching (t, cases)) ->
      assert false (* TODO *)
  | (loc, PretypedTree.Let (name, t1, t2)) ->
      assert false (* TODO *)
  | (loc, PretypedTree.LetRec (name, ty, t1, t2)) ->
      assert false (* TODO *)
  | (_, PretypedTree.Fail _) ->
      assert false (* TODO *)
  | (_, PretypedTree.Try _) ->
      assert false (* TODO *)
  | (loc, PretypedTree.Annot (t, (ty, eff))) ->
      let (t, ty', eff') = check_term options env t in
      let ty = check_type ~loc env ty' ty in
      let eff = check_eff_opt ~loc env eff' eff in
      (t, ty, eff)
  | (_, PretypedTree.Const c) ->
      let (c, ty) = get_const options c in
      (Const c, TypedEnv.NTy ty, [])

let check_eff_value ~current_module options name ty eff =
  let loc = Ident.Name.loc name in
  if options#with_main && is_main ~current_module name then begin
    if not (List.for_all (NType.is_subset_of (io options)) eff) then
      Err.fail ~loc
        "Effects different than 'IO' are not allowed in the main function";
    if not (NType.is_subset_of ty (unit options)) then
      Err.fail_doc ~loc
        Utils.PPrint.(str "The main function is supposed to have type 'unit' \
                           but got type" ^^^
                      squotes (NType.dump ty) ^^^
                      dot);
    true
  end else begin
    if not (List.is_empty eff) then
      Err.fail ~loc "Effects are not allowed on toplevel";
    false
  end

let rec get_foreign_type ~default options = function
  | TypedEnv.NForall (_, _, ty) ->
      get_foreign_type ~default options ty
  | TypedEnv.NTy name ->
      let arg_ty_map =
        [ (Builtins.int options, `Int ())
        ; (Builtins.float options, `Float ())
        ; (Builtins.char options, `Char ())
          (* NOTE: String is not present because it is a pointer *)
        ]
      in
      Option.get_or ~default
        (List.Assoc.get ~eq:Ident.Type.equal name arg_ty_map)
  | TypedEnv.NApp _ | TypedEnv.NFun _ ->
      default

let rec is_last = function
  | TypedEnv.NForall (_, _, t) -> is_last t
  | TypedEnv.NTy _ | TypedEnv.NApp _ -> true
  | TypedEnv.NFun _ -> false

let rec check_foreign_type ~loc is_first options env = function
  | TypedEnv.NForall (_, _, ty) ->
      check_foreign_type ~loc true options env ty
  | TypedEnv.NTy _ | TypedEnv.NApp _ ->
      Err.fail ~loc "Cannot bind a global variable"
  | TypedEnv.NFun (t1, e, t2) when is_last t2 ->
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
  | TypedEnv.NFun (t1, _, t2) ->
      (* TODO: Warning if e <> [] ? *)
      let t1 = get_foreign_type ~default:`Custom options t1 in
      let (ret, t2) = check_foreign_type ~loc false options env t2 in
      (ret, [t1] @ t2)

let check_top ~current_module options (acc, has_main, env) = function
  | PretypedTree.Value (name, t) ->
      let (t, ty, eff) = check_term options env t in
      let has_main = check_eff_value ~current_module options name ty eff in
      let acc = acc @ [Value (name, t)] in
      let env = Env.add_toplevel_value name ty env in
      (acc, has_main, env)
  | PretypedTree.Type (name, ty) ->
      let env = Env.add_type_alias name ty env in
      (acc, has_main, env)
  | PretypedTree.Foreign (cname, name, ty) ->
      let ty = NType.check ~pure_arrow:`Partial env ty in
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
        let ty = NType.check ~pure_arrow:`Partial env ty in
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
