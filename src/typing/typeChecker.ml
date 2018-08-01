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
  Option.map_or ~default:eff' (check_eff ~loc env eff')

let unit options = TypedEnv.NTy (Builtins.unit options)
let is_main ~current_module = Ident.Name.equal (Builtins.main ~current_module)
let io options = TypedEnv.NTy (Builtins.io options)
let has_io options = List.exists (NType.is_subset_of (io options))

let get_const options = function
  | PretypedTree.Int n -> (`Int n, Builtins.int options)
  | PretypedTree.Float n -> (`Float n, Builtins.float options)
  | PretypedTree.Char c -> (`Char c, Builtins.char options)
  | PretypedTree.String s -> (`String s, Builtins.string options)

let get_rep name = function
  | TypedEnv.Index idx -> Index idx
  | TypedEnv.Exn -> Exn (Ident.Constr.to_name name)

let eff_of_ty ~loc options = function
  | TypedEnv.NSum [] ->
      assert false (* TODO: Check this. This shouldn't happen *)
  | TypedEnv.NSum sum ->
      let exn = TypedEnv.NTy (Builtins.exn options) in
      List.map (fun t -> TypedEnv.NApp (exn, NType.to_type t)) sum
  | TypedEnv.NTy _ | TypedEnv.NFun _
  | TypedEnv.NForall _ | TypedEnv.NApp _ as t ->
      Err.fail_doc
        ~loc
        Utils.PPrint.(str "Cannot throw exception of type" ^^^
                      squotes (NType.dump t) ^^^
                      dot)

let unify =
  let rec aux ty eff = function
    | [] ->
        (ty, eff)
    | (loc, ty', eff')::tys ->
        if NType.is_subset_of ty ty' then
          aux ty' (eff @ eff') tys
        else if NType.is_subset_of ty' ty then
          aux ty (eff @ eff') tys
        else
          type_fail ~loc ~has:ty' ~expected:ty
  in
  function
  | [] -> assert false (* NOTE: Shouldn't happen *)
  | (_, ty, eff)::tys -> aux ty eff tys

let unify_list l1 =
  let error name =
    Err.fail
      ~loc:(Ident.Name.loc name)
      "Variable '%s' is bound several times in this matching"
      (Ident.Name.to_string name)
  in
  let rec aux l1 = function
    | [] -> begin match l1 with
      | [] -> []
      | (name, _)::_ -> error name
    end
    | (name, ty)::l2 ->
        let eq = Ident.Name.equal name in
        begin match List.partition (fun (name', _) -> eq name') l1 with
        | [], _ ->
            error name
        | [(name', ty')], l1 ->
            let ty = (Ident.Name.loc name, ty, []) in
            let ty' = (Ident.Name.loc name', ty', []) in
            (name, fst (unify [ty; ty']))::aux l1 l2
        | _::(name, _)::_, _ ->
            error name
        end
  in
  aux l1

let rec filter_effects options eff = function
  | [] ->
      eff
  | ((name, _), _)::branches ->
      (* TODO: Check duplication *)
      let exn = TypedEnv.NTy (Builtins.exn options) in
      let ty' = TypedEnv.NApp (exn, TypedEnv.Ty (Ident.Constr.to_type name)) in
      let eff = List.filter (fun ty -> not (NType.is_subset_of ty ty')) eff in
      filter_effects options eff branches

let rec get_constrs ~loc env ty = match Type.head ty with
  | None ->
      Err.fail_doc
        ~loc
        Utils.PPrint.(str "The type" ^^^
                      squotes (Type.dump ty) ^^^
                      str "cannot be matched.")
  | Some ty ->
      begin match EnvMap.Type.find ty env.TypedEnv.types with
      | TypedEnv.Abstract _ ->
          Err.fail
            ~loc
            "Cannot match over the abstract type '%s'"
            (Ident.Type.to_string ty)
      | TypedEnv.Alias (_, ty) ->
          get_constrs ~loc env ty
      | TypedEnv.Datatype (_, constrs) ->
          constrs
      end

let fold_ty_list ~loc f x ~has ~expected =
  try List.fold_left2 f x has expected with
  | Invalid_argument _ ->
      Err.fail
        ~loc
        "Wrong number of parameters. Has %d but expected %d."
        (List.length has)
        (List.length expected)

let pattern_to_matrix env base_ty =
  let rec aux ty = function
    | PretypedTree.TyConstr (loc, c, ps) ->
        p_constr ~loc env ty c ps
    | PretypedTree.Wildcard ->
        ((ty, Pattern.Wildcard), [])
    | PretypedTree.Or (p1, p2) ->
        let (p1, tys1) = aux ty p1 in
        let (p2, tys2) = aux ty p2 in
        let tys = unify_list tys1 tys2 in
        ((ty, Pattern.Or (p1, p2)), tys)
    | PretypedTree.As (p, name) ->
        let (p, tys) = aux ty p in
        ((ty, Pattern.As (p, name)), (name, ty)::tys)
  and p_constr ~loc env ty c ps =
    let constrs = get_constrs ~loc env (NType.to_type ty) in
    match List.find_all (fun (c', _, _) -> Ident.Constr.equal c c') constrs with
    | [] ->
        Err.fail_doc
          ~loc
          Utils.PPrint.(squotes (str (Ident.Constr.to_string c)) ^^^
                        str "is not a constructor of" ^^^
                        squotes (NType.dump base_ty) ^^
                        dot)
    | [(_, _, ty')] ->
        let (expected, _) = NType.match_ty ~base_ty ty' in
        let fold = fold_ty_list ~loc in
        let (args, tys) = fold fold_arg ([], []) ~has:ps ~expected in
        ((ty, Pattern.Constr (loc, c, args)), tys)
    | _::_::_ ->
        assert false (* NOTE: This is forbidden by Env.map_variants *)
  and fold_arg (args, tys) p ty =
    let (p, tys1) = aux ty p in
    (args @ [p], tys @ tys1)
  in
  fun (p, a) ->
    let (p, l) = aux base_ty p in
    (([p], a), l)

let patterns_to_matrix env ty patterns =
  let m = List.map (pattern_to_matrix env ty) patterns in
  List.split m

let split_patterns patterns =
  let (patterns, results) = List.split patterns in
  let aux a pattern = (pattern, a) in
  (List.mapi aux patterns, results)

let pat_vars =
  let (%) = Fun.(%) in
  let aux s (name, _) = Ident.Name.Set.add name s in
  List.fold_left aux Ident.Name.Set.empty % List.flatten

let rec check_decision_tree ~loc env = function
  | Pattern.Switch (cases, default) -> check_switch ~loc env cases default
  | Pattern.Swap (idx, tree) -> Swap (idx, check_decision_tree ~loc env tree)
  | Pattern.Alias (name, tree) -> Alias (name, check_decision_tree ~loc env tree)
  | Pattern.Jump a -> Jump a
and check_switch ~loc env cases default =
  let ty = List.map (fun (loc, _, ty, _) -> (loc, ty, [])) cases in
  let ty, _ = unify ty in
  let constrs =
    try get_constrs ~loc:Builtins.unknown_loc env (NType.to_type ty) with
    | Err.Exn _ -> assert false (* NOTE: already checked in pattern_to_matrix *)
  in
  let has_default = Option.is_some default in
  let cases = check_cases ~loc ~has_default env cases constrs in
  Switch (cases, Option.map (check_decision_tree ~loc env) default)
and check_cases ~loc ~has_default env cases = function
  | [] when List.is_empty cases ->
      []
  | [] ->
      assert false
  | (name, idx, ty)::constrs ->
      let eq = Ident.Constr.equal name in
      begin match List.find_all (fun (_, c, _, _) -> eq c) cases with
      | [] when has_default ->
          check_cases ~loc ~has_default env cases constrs
      | [] ->
          Err.fail
            ~loc
            "This pattern matching is not exhaustive. \
             This constructor is not matched: %s"
            (Ident.Constr.to_string name)
      | [(loc', _, _, tree)] ->
          let len = NType.size ty in
          let tree = check_decision_tree ~loc:loc' env tree in
          let cases = List.filter (fun (_, c, _, _) -> not (eq c)) cases in
          let constrs = check_cases ~loc ~has_default env cases constrs in
          (Index idx, len, tree)::constrs
      | _::(loc, _, _, _)::_ ->
          Err.fail ~loc "This match case is unused."
      end

let rec check_try_branch options env ((name, args), t) =
  match EnvMap.Constr.find name env.TypedEnv.constrs with
  | TypedEnv.Exn, ty' ->
      let (args', _) = NType.monomorphic_split ty' in
      let aux env name (ty, _) = Env.add_value name ty env in
      let env =
        let loc = Ident.Constr.loc name in
        fold_ty_list ~loc aux env ~has:args ~expected:args'
      in
      let loct = fst t in
      let (t, tyt, eff) = check_term options env t in
      (((Ident.Constr.to_name name, args), t), (loct, tyt, eff))
  | TypedEnv.Index _, _ ->
      Err.fail
        ~loc:(Ident.Constr.loc name)
        "This data constructor is not an exception"

and check_try_branches options ty eff env = function
  | [] ->
      assert false (* NOTE: This is forbidden by the syntax *)
  | branches ->
      let eff = filter_effects options eff branches in
      let branches = List.map (check_try_branch options env) branches in
      let (branches, tys) = List.split branches in
      let (ty, eff) = unify ((Builtins.unknown_loc, ty, eff)::tys) in
      (branches, ty, eff)

and check_results options env vars results =
  let check_vars env (results, ty) vars result =
    let check_var (vars, env) (name, ty) =
      match List.find_opt (Ident.Name.equal name) vars with
      | Some name when not (Ident.Name.equal name Builtins.underscore) ->
          let loc = Ident.Name.loc name in
          Err.fail ~loc "Variable already defined in this pattern."
      | None | Some _ ->
          (name :: vars, Env.add_value name ty env)
    in
    let (_, env) = List.fold_left check_var ([], env) vars in
    let (t, ty1, eff1) = check_term options env result in
    (results @ [t], ty @ [(fst result, ty1, eff1)])
  in
  let (results, tys) =
    try List.fold_left2 (check_vars env) ([], []) vars results with
    | Invalid_argument _ ->
        assert false (* NOTE: Not possible (see pattern_to_matrix) *)
  in
  (results, unify tys)

(* TODO: Use Set instead of list to encode the effects *)
and check_term options env = function
  | (_, PretypedTree.Abs ((name, ty), t)) ->
      let ty = NType.check ~pure_arrow:`Forbid env ty in
      let env = Env.add_value name ty env in
      let (t, ty', eff) = check_term options env t in
      (Abs (name, t), TypedEnv.NFun (ty, eff, ty'), [])
  | (loc, PretypedTree.TAbs ((name, k), t)) ->
      let env = Env.add_abstract_type name k env in
      let (t, ty, eff) = check_term options env t in
      if not (List.is_empty eff) then
        Err.fail ~loc "Effects are forbidden under type abstractions.";
      (t, TypedEnv.NForall (name, k, ty), [])
  | (_, PretypedTree.CAbs _) ->
      assert false (* TODO *)
  | (loc, PretypedTree.App (t1, t2)) ->
      let (t1, ty1, eff1) = check_term options env t1 in
      let (t2, ty2, eff2) = check_term options env t2 in
      let (ty, eff3) = app ~loc ty2 ty1 in
      (App (t1, t2), ty, eff1 @ eff2 @ eff3)
  | (loc, PretypedTree.TApp (t, ty)) ->
      let (t, ty', eff) = check_term options env t in
      let ty = Type.check ~pure_arrow:`Forbid env ty in
      let ty = tapp ~loc ty ty' in
      (t, ty, eff)
  | (_, PretypedTree.CApp _) ->
      assert false (* TODO *)
  | (_, PretypedTree.Val name) ->
      let ty = EnvMap.Value.find name env.TypedEnv.values in
      (Val name, ty, [])
  | (_, PretypedTree.Var name) ->
      let (rep, ty) = EnvMap.Constr.find name env.TypedEnv.constrs in
      let rep = get_rep name rep in
      let size = NType.size ty in
      (Var (rep, size), ty, [])
  | (loc, PretypedTree.PatternMatching (t, patterns)) ->
      let (t, ty, eff1) = check_term options env t in
      let (patterns, results) = split_patterns patterns in
      let (matrix, vars) = patterns_to_matrix env ty patterns in
      let tree = Pattern.compile matrix in
      let tree = check_decision_tree ~loc env tree in
      let (results, (ty, eff2)) = check_results options env vars results in
      let vars = pat_vars vars in
      (PatternMatching (t, vars, results, tree), ty, eff1 @ eff2)
  | (_, PretypedTree.Let (name, t1, t2)) ->
      let (t1, ty1, eff1) = check_term options env t1 in
      let env = Env.add_value name ty1 env in
      let (t2, ty2, eff2) = check_term options env t2 in
      (Let (name, t1, t2), ty2, eff1 @ eff2)
  | (loc, PretypedTree.LetRec (name, ty, t1, t2)) ->
      let ty = NType.check ~pure_arrow:`Partial env ty in
      let env = Env.add_value name ty env in
      let (t1, ty1, eff1) = check_term options env t1 in
      (* NOTE: ty is already checked by Annot (see Pretyper) *)
      let (t2, ty2, eff2) = check_term options env t2 in
      (LetRec (name, t1, t2), ty2, eff1 @ eff2)
  | (loc, PretypedTree.Fail (ty, t)) ->
      let ty = NType.check ~pure_arrow:`Allow env ty in
      let (t, ty', eff') = check_term options env t in
      let eff = eff_of_ty ~loc options ty' in
      (Fail t, ty, eff @ eff')
  | (_, PretypedTree.Try (t, branches)) ->
      let (t, ty, eff) = check_term options env t in
      let (branches, ty, eff) = check_try_branches options ty eff env branches in
      (Try (t, branches), ty, eff)
  | (loc, PretypedTree.Annot (t, (ty, eff))) ->
      let (t, ty', eff') = check_term options env t in
      let ty = check_type ~loc env ty' ty in
      let eff = check_eff_opt ~loc env eff' eff in
      (t, ty, eff)
  | (_, PretypedTree.Const c) ->
      let (c, ty) = get_const options c in
      (Const c, TypedEnv.NTy ty, [])

and app ~loc ty2 = function
  | TypedEnv.NFun (ty2', eff, ty) when NType.is_subset_of ty2 ty2' ->
      (ty, eff)
  | TypedEnv.NFun (expected, _, _) ->
      type_fail ~loc ~has:ty2 ~expected
  | TypedEnv.NTy _ | TypedEnv.NSum _ | TypedEnv.NForall _ | TypedEnv.NApp _ as ty ->
      Err.fail_doc
        ~loc
        Utils.PPrint.(str "This expression has type" ^^^
                      squotes (NType.dump ty) ^/^
                      str "This is not a function; it cannot be applied.")

and tapp ~loc (ty, k) = function
  | TypedEnv.NForall (name, k', ty') when Type.kind_equal k k' ->
      NType.replace name ~by:ty ty'
  | TypedEnv.NForall (_, expected, _) ->
      Type.kind_fail ~loc ~has:k ~expected
  | TypedEnv.NTy _ | TypedEnv.NSum _ | TypedEnv.NFun _ | TypedEnv.NApp _ as ty ->
      Err.fail_doc
        ~loc
        Utils.PPrint.(str "This expression has type" ^^^
                      squotes (NType.dump ty) ^/^
                      str "This is not a type abstraction; it cannot be applied.")

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

let rec get_foreign_type map options = function
  | TypedEnv.NForall (_, _, ty) ->
      get_foreign_type map options ty
  | TypedEnv.NTy name ->
      let arg_ty_map =
        [ (Builtins.int options, `Int ())
        ; (Builtins.float options, `Float ())
        ; (Builtins.char options, `Char ())
          (* NOTE: String is not present because it is a pointer *)
        ]
      in
      let arg_ty_map = arg_ty_map @ map in
      Option.get_or ~default:`Custom
        (List.Assoc.get ~eq:Ident.Type.equal name arg_ty_map)
  | TypedEnv.NSum _ | TypedEnv.NApp _ | TypedEnv.NFun _ ->
      `Custom

let get_arg_foreign_type options ty =
  get_foreign_type [] options ty

let get_ret_foreign_type options ty =
  get_foreign_type [(Builtins.unit options, `Void)] options ty

let rec check_foreign_type ~loc options env = function
  | [], _ ->
      Err.fail ~loc "Cannot bind a global variable"
  | [(t1, e)], t2 ->
      if not (has_io options e) then
        Err.fail ~loc "Bindings cannot be pure. All bindings have \
                       to use the IO effect on the final arrow";
      let t1 = get_arg_foreign_type options t1 in
      let t2 = get_ret_foreign_type options t2 in
      (t2, [t1])
  | (t1, _) :: l, t2 ->
      (* TODO: Warning if e <> [] ? *)
      let t1 = get_arg_foreign_type options t1 in
      let (ret, t2) = check_foreign_type ~loc options env (l, t2) in
      (ret, [t1] @ t2)

let check_top ~current_module options (acc, has_main, env) = function
  | PretypedTree.Value (name, t) ->
      let (t, ty, eff) = check_term options env t in
      let has_main = check_eff_value ~current_module options name ty eff in
      let acc = acc @ [Value (name, t)] in
      let env = Env.add_value name ty env in
      (acc, has_main, env)
  | PretypedTree.Type (name, ty) ->
      let env = Env.add_type_alias name ty env in
      (acc, has_main, env)
  | PretypedTree.Foreign (cname, name, ty) ->
      let ty = NType.check ~pure_arrow:`Partial env ty in
      let rty =
        check_foreign_type ~loc:(Ident.Name.loc name) options env (NType.monomorphic_split ty)
      in
      let acc = acc @ [Foreign (cname, name, rty)] in
      let env = Env.add_value name ty env in
      (acc, has_main, env)
  | PretypedTree.Datatype (name, k, variants) ->
      let env = Env.add_datatype name k variants env in
      (acc, has_main, env)
  | PretypedTree.Exception (name, args) ->
      let acc = acc @ [Exception (Ident.Constr.to_name name)] in
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

let check_interface =
  let aux env = function
    | PretypedTree.IVal (name, ty) ->
        let ty = NType.check ~pure_arrow:`Partial env ty in
        Env.add_value name ty env
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
