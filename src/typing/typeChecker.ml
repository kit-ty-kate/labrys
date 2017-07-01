(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

open UntypedTree

let check_type options ~loc_t ~ty:(ty, eff) ~ty_t ~effects env =
  let ty = Types.of_parse_tree ~pure_arrow:`Allow options env ty in
  if not (Types.is_subset_of ty_t ty) then
    Types.TyErr.fail ~loc_t ~has:ty_t ~expected:ty;
  begin match eff with
  | Some eff ->
      let eff = Effects.of_list options env eff in
      if not (Effects.is_subset_of [] effects eff) then
        Err.fail
          ~loc:loc_t
          "This expression has the effect %s but expected the effect %s"
          (Effects.to_string effects)
          (Effects.to_string eff);
  | None ->
      ()
  end

let check_effects_forall ~loc_t options ~effect =
  (* TODO: Do I need only that to ensure type soundness with side effects ?
     Do I also need to check for exceptions ? *)
  if Effects.has_io options effect then
    Err.fail ~loc:loc_t "Cannot have IO effects under a forall"

let get_const options = function
  | PretypedTree.Int n -> (Int n, Builtins.int options)
  | PretypedTree.Float n -> (Float n, Builtins.float options)
  | PretypedTree.Char c -> (Char c, Builtins.char options)
  | PretypedTree.String s -> (String s, Builtins.string options)

let wrap_typeclass_apps ~loc env ~tyclasses ~f g =
  let rec aux f n = function
    | None::xs ->
        let name =
          Ident.Name.local_create ~loc:Builtins.unknown_loc "tyclass"
        in
        let name = Ident.Name.unique name n in
        Abs (name, aux (App (f, Val name)) (succ n) xs)
    | Some (tyclass, tys)::xs ->
        let tyclass' = EnvMap.TyClass.find tyclass env.Env.tyclasses in
        let name = Class.get_instance_name ~loc ~tyclass tys tyclass' in
        aux (App (f, Val name)) n xs
    | [] ->
        g f
  in
  aux f 1 tyclasses

let rec aux options env = function
  | (_, PretypedTree.Abs ((name, ty), t)) ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options env ty in
      let env = Env.add_value name ty env in
      let (expr, ty_expr, effect) = aux options env t in
      let abs_ty = PrivateTypes.Fun (ty, effect, ty_expr) in
      (Abs (name, expr), abs_ty, Effects.empty)
  | (_, PretypedTree.TAbs ((name, k), t)) ->
      let env = Env.add_type_var name k env in
      let (expr, ty_expr, effect) = aux options env t in
      check_effects_forall ~loc_t:(fst t) options ~effect;
      let abs_ty = Types.forall (name, k, ty_expr) in
      (expr, abs_ty, effect)
  | (_, PretypedTree.CAbs ((name, (tyclass, tyvars, args)), t)) ->
      let tyclass' = EnvMap.TyClass.find tyclass env.Env.tyclasses in
      let tyvars =
        let aux acc (name, k) = EnvMap.TypeVar.add name k acc in
        List.fold_left aux EnvMap.TypeVar.empty tyvars
      in
      let (env, args) =
        let loc = Ident.TyClass.loc tyclass in
        let f = Types.of_parse_tree_kind ~pure_arrow:`Forbid options in
        Class.get_params ~loc f env tyvars args tyclass'
      in
      let env = Env.add_named_instance name (tyclass, args) env in
      let (expr, ty_expr, effect) = aux options env t in
      let abs_ty = Types.tyclass ((tyclass, tyvars, args), effect, ty_expr) in
      (Abs (Ident.Instance.to_name name, expr), abs_ty, Effects.empty)
  | (loc, PretypedTree.App (f, x)) ->
      let loc_f = fst f in
      let loc_x = fst x in
      let (f, ty_f, effect1) = aux options env f in
      let (x, ty_x, effect2) = aux options env x in
      let (effect3, res, ty_x) = Types.apply ~loc_f ~loc_x ty_f ty_x in
      let (tyclasses, eff_f, res) = Types.extract_filled_tyclasses res in
      let (tyclasses_x, eff_x, _) = Types.extract_filled_tyclasses ty_x in
      let f =
        let aux f =
          let x =
            let aux x = function
              | None ->
                  assert false
              | Some (tyclass, tys) ->
                  let tyclass' = EnvMap.TyClass.find tyclass env.Env.tyclasses in
                  let name = Class.get_instance_name ~loc ~tyclass tys tyclass' in
                  App (x, Val name)
            in
            List.fold_left aux x tyclasses_x
          in
          App (f, x)
        in
        wrap_typeclass_apps ~loc env ~tyclasses ~f aux
      in
      (f, res, Effects.union5 effect1 effect2 effect3 eff_f eff_x)
  | (loc, PretypedTree.TApp (f, ty_x)) ->
      let loc_f = fst f in
      let loc_x = fst ty_x in
      let (f, ty_f, effect) = aux options env f in
      let (ty_x, kx) = Types.of_parse_tree_kind ~pure_arrow:`Allow options env ty_x in
      let res = Types.apply_ty ~loc_f ~loc_x ~ty_x ~kind_x:kx ty_f in
      let (tyclasses, eff_f, res) = Types.extract_filled_tyclasses res in
      let f = wrap_typeclass_apps ~loc env ~tyclasses ~f Fun.id in
      (f, res, Effects.union effect eff_f)
  | (loc, PretypedTree.CApp (f, x)) ->
      let (f, ty_f, effect1) = aux options env f in
      let (name, tyclass, args) = match x with
        | PretypedTree.TyClassVariable name ->
            let (tyclass, args) =
              EnvMap.Instance.find name env.Env.named_instances
            in
            (Ident.Instance.to_name name, tyclass, args)
        | PretypedTree.TyClassInstance (tyclass, tys) ->
            let aux x = fst (Types.of_parse_tree_kind ~pure_arrow:`Allow options env x) in
            let tys = List.map aux tys in
            let tyclass' = EnvMap.TyClass.find tyclass env.Env.tyclasses in
            let name =
              Class.get_instance_name ~loc:(Ident.TyClass.loc tyclass) ~tyclass tys tyclass'
            in
            (name, tyclass, tys)
      in
      (* TODO: Fix loc *)
      let (res, effect2) = Types.apply_tyclass ~loc_x:loc ty_f tyclass args in
      let (tyclasses, eff_f, res) = Types.extract_filled_tyclasses res in
      let f = App (f, Val name) in
      let f = wrap_typeclass_apps ~loc env ~tyclasses ~f Fun.id in
      (f, res, Effects.union3 effect1 effect2 eff_f)
  | (_, PretypedTree.Val name) ->
      let ty = EnvMap.Value.find name env.Env.values in
      (Val name, ty, Effects.empty)
  | (_, PretypedTree.Var name) ->
      let (idx, ty, len) =
        EnvMap.Variant.find name env.Env.variants
      in
      (Var (idx, len), ty, Effects.empty)
  | (loc, PretypedTree.PatternMatching (t, patterns)) ->
      let loc_t = fst t in
      let (t, ty, effect1) = aux options env t in
      if not (Types.is_value ty) then
        Err.fail ~loc:loc_t "This value cannot be matched";
      let (patterns, results, initial_ty, effect2) =
        Pattern.create ~loc (aux options) env ty patterns
      in
      let effect = Effects.union effect1 effect2 in
      (PatternMatching (t, results, Unreachable, patterns), initial_ty, effect)
  | (_, PretypedTree.Let (name, t, xs)) ->
      let (t, ty_t, effect1) = aux options env t in
      let env = Env.add_value name ty_t env in
      let (xs, ty_xs, effect2) = aux options env xs in
      (Let (name, t, xs), ty_xs, Effects.union effect1 effect2)
  | (_, PretypedTree.LetRec (name, ty, t, xs)) ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options env ty in
      let env = Env.add_value name ty env in
      let (t, _, effect1) = aux options env t in
      (* NOTE: We doesn't need to check the type of "t" here because it has
         already been checked by an Annot (as it is mandatory for recursive
         values) *)
      let (xs, ty_xs, effect2) = aux options env xs in
      (LetRec (name, t, xs), ty_xs, Effects.union effect1 effect2)
  | (loc, PretypedTree.Fail (ty, (exn, args))) ->
      let tys = EnvMap.Exn.find exn env.Env.exceptions in
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options env ty in
      let (args, effects) =
        let aux (acc, effects) ty_exn arg =
          let loc_arg = fst arg in
          let (arg, ty_arg, eff) = aux options env arg in
          if not (Types.is_subset_of ty_arg ty_exn) then
            Types.TyErr.fail ~loc_t:loc_arg ~has:ty_arg ~expected:ty_exn;
          (arg :: acc, Effects.union eff effects)
        in
        try List.fold_left2 aux ([], Effects.empty) tys args with
        | Invalid_argument _ ->
            Err.fail
              ~loc
              "Cannot apply an exception partially"
      in
      let args = List.rev args in
      (Fail (exn, args), ty, Effects.add_exn exn effects)
  | (_, PretypedTree.Try (e, branches)) ->
      let (e, ty, effect) = aux options env e in
      let (branches, effect) =
        let aux (branches, effect) ((name, args), t) =
          let tys = EnvMap.Exn.find name env.Env.exceptions in
          if Int.(List.length args <> List.length tys) then
            Err.fail ~loc:(Ident.Exn.loc name) "Wrong number of argument";
          (branches @ [((name, args), t)], Effects.remove_exn name effect)
        in
        List.fold_left aux ([], effect) branches
      in
      let aux (acc, effect) ((name, args), t) =
        let loc_t = fst t in
        let (t, ty', eff) = aux options env t in
        (* TODO: Replace this pattern by Types.Check.is_subset_of *)
        (* TODO: This should take the larger type, not only the fist one *)
        if not (Types.is_subset_of ty' ty) then
          Types.TyErr.fail ~loc_t ~has:ty' ~expected:ty;
        (((name, args), t) :: acc, Effects.union eff effect)
      in
      let (branches, effect) = List.fold_left aux ([], effect) branches in
      let branches = List.rev branches in
      (Try (e, branches), ty, effect)
  | (_, PretypedTree.Annot (t, ty)) ->
      let loc_t = fst t in
      let (_, ty_t, effects) as res = aux options env t in
      check_type options ~loc_t ~ty ~ty_t ~effects env;
      res
  | (loc, PretypedTree.Const const) ->
      let (const, ty) = get_const options const in
      (Const const, Types.ty ~loc env ty, Effects.empty)

let transform_variants options ~datatype ~ty_args ~args env =
  let env' = List.fold_left (fun env (name, k) -> Env.add_type_var name k env) env args in
  let rec aux index = function
    | (name, tys, ty) :: xs ->
        let tys = List.map (Types.of_parse_tree ~pure_arrow:`Allow options env') tys in
        let ty = Types.of_parse_tree ~pure_arrow:`Allow options env ty in
        let env = aux (succ index) xs in
        let env = Env.add_variant name (index, ty, List.length tys) env in
        Env.add_constr datatype name ty_args (tys, index) env
    | [] ->
        env
  in
  aux 0

let is_main ~current_module ~has_main x =
  let b = Ident.Name.equal x (Builtins.main ~current_module) in
  if has_main && b then
    Err.fail ~loc:(Ident.Name.loc x) "There must be only one main";
  b

let check_effects ~current_module ~with_main ~has_main ~name options (t, ty, effects) =
  let is_main = with_main && is_main ~current_module ~has_main name in
  if not (is_main || Effects.is_empty effects) then
    Err.fail ~loc:(Ident.Name.loc name) "Effects are not allowed on toplevel";
  if is_main && not (Types.is_unit options ty) then
    Err.fail
      ~loc:(Ident.Name.loc name)
      "The main must have type '%s' but has type '%s'"
      (Ident.Type.to_string (Builtins.unit options))
      (Types.to_string ty);
  (is_main, t, ty)

let from_value ~current_module ~with_main ~has_main options env (name, term) =
  let (has_main, x, ty_t) = check_effects ~current_module ~with_main ~has_main ~name options (aux options env term) in
  let env = Env.add_value name ty_t env in
  ((name, x), ty_t, has_main, env)

let get_foreign_type ~loc options =
  (* TODO: Unit -> X   -->   args = [] *)
  let fail loc =
    Err.fail ~loc "Such type is not handle in foreign declarations"
  in
  let fail_complex ty =
    Err.fail
      ~loc
      "Foreign declaration doesn't handle complexe types '%s'"
      (PrivateTypes.ty_to_string ty)
  in
  let arg_ty_map =
    [ (Builtins.int options, Int ())
    ; (Builtins.float options, Float ())
    ; (Builtins.char options, Char ())
    ; (Builtins.string options, String ())
    ]
  in
  let ret_ty_map =
    [ (Builtins.unit options, Void)
    ]
    @ List.map (fun (k, x) -> (k, Alloc x)) arg_ty_map
  in
  let rec aux acc = function
    | PrivateTypes.Fun (PrivateTypes.Ty name, eff, t) ->
        let x =
          match List.Assoc.get ~eq:Ident.Type.equal name arg_ty_map with
          | Some x -> x
          | None -> fail (Ident.Type.loc name)
        in
        begin match t with
        | PrivateTypes.Fun _ ->
            if not (Effects.is_empty eff) then
              Err.fail
                ~loc
                "Only empty effects are allowed on a non-final arrow";
            aux (x :: acc) t
        | PrivateTypes.Ty name ->
            if not (Effects.has_io options eff) then
              Err.fail
                ~loc
                "Bindings cannot be pure. \
                 All bindings have to use the IO effect on the final arrow";
            begin match List.Assoc.get ~eq:Ident.Type.equal name ret_ty_map with
            | Some y -> (y, List.rev (x :: acc))
            | None -> fail (Ident.Type.loc name)
            end
        | PrivateTypes.Eff _
        | PrivateTypes.Forall _
        | PrivateTypes.TyClass _
        | PrivateTypes.AbsOnTy _
        | PrivateTypes.AppOnTy _ ->
            fail_complex t
        | PrivateTypes.TyVar _ ->
            assert false
        end
    | PrivateTypes.Ty _ ->
        Err.fail ~loc "Cannot bind a global variable. Too unsafe."
    | PrivateTypes.Fun _
    | PrivateTypes.Eff _
    | PrivateTypes.Forall _
    | PrivateTypes.TyClass _
    | PrivateTypes.AbsOnTy _
    | PrivateTypes.AppOnTy _ as ty ->
        fail_complex ty
    | PrivateTypes.TyVar _ ->
        assert false
  in
  aux []

let rec from_parse_tree ~current_module ~with_main ~has_main options env = function
  | PretypedTree.Value value :: xs ->
      let (value, _, has_main, env) = from_value ~current_module ~with_main ~has_main options env value in
      let (xs, has_main, env) = from_parse_tree ~current_module ~with_main ~has_main options env xs in
      (Value value :: xs, has_main, env)
  | PretypedTree.Type (name, ty) :: xs ->
      let ty = Types.of_parse_tree_kind ~pure_arrow:`Forbid options env ty in
      let env = Env.add_type name (Types.Alias ty) env in
      from_parse_tree ~current_module ~with_main ~has_main options env xs
  | PretypedTree.Foreign (cname, name, ty) :: xs ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options env ty in
      let env = Env.add_value name ty env in
      let ty = get_foreign_type ~loc:(Ident.Name.loc name) options ty in
      let (xs, has_main, env) = from_parse_tree ~current_module ~with_main ~has_main options env xs in
      (Foreign (cname, name, ty) :: xs, has_main, env)
  | PretypedTree.Datatype (name, kind, args, variants) :: xs ->
      let ty_args = List.map fst args in
      let env = Env.add_type name (Types.Abstract kind) env in
      let env = transform_variants options ~datatype:name ~ty_args ~args env variants in
      from_parse_tree ~current_module ~with_main ~has_main options env xs
  | PretypedTree.Exception (name, args) :: xs ->
      let args = List.map (Types.of_parse_tree ~pure_arrow:`Forbid options env) args in
      let env = Env.add_exception name args env in
      let (xs, has_main, env) = from_parse_tree ~current_module ~with_main ~has_main options env xs in
      (Exception name :: xs, has_main, env)
  | PretypedTree.Class (name, params, sigs) :: xs ->
      let sigs =
        let env =
          let aux env (name, k) = Env.add_type_var name k env in
          List.fold_left aux env params
        in
        let aux (name, ty) =
          (name, Types.of_parse_tree ~pure_arrow:`Forbid options env ty)
        in
        List.map aux sigs
      in
      let tyclass = Class.create params sigs in
      let env = Env.add_tyclass name tyclass env in
      let env =
        let aux env (name_sig, ty) =
          let ty = Types.tyclass_wrap name params ty in
          Env.add_value name_sig ty env
        in
        List.fold_left aux env sigs
      in
      let (xs, has_main, env) = from_parse_tree ~current_module ~with_main ~has_main options env xs in
      let (_, xs) =
        let aux (n, xs) (name, _) =
          let abs_name = Ident.Name.create ~loc:Builtins.unknown_loc current_module "0" in
          (succ n, Value (name, Abs (abs_name, RecordGet (Val abs_name, n))) :: xs)
        in
        List.fold_left aux (0, xs) sigs
      in
      (xs, has_main, env)
  | PretypedTree.Instance ((tyclass, tys), name, values) :: xs ->
      let tyclass' = EnvMap.TyClass.find tyclass env.Env.tyclasses in
      let tys = List.map (Types.of_parse_tree_kind ~pure_arrow:`Forbid options env) tys in
      let values =
        let aux value =
          let (value, ty, _, _) = from_value ~current_module ~with_main ~has_main options env value in
          (value, ty)
        in
        List.map aux values
      in
      let (name', tys, tyclass') = Class.add_instance ~tyclass ~current_module tys tyclass' in
      let env = match name with
        | Some name -> Env.add_named_instance name (tyclass, tys) env
        | None -> env
      in
      let env = Env.replace_tyclass tyclass tyclass' env in
      let values = Class.get_values ~loc:(Ident.TyClass.loc tyclass) tys values tyclass' in
      let (xs, has_main, env) = from_parse_tree ~current_module ~with_main ~has_main options env xs in
      let xs = Option.map_or ~default:xs (fun name -> Value (Ident.Instance.to_name name, Val name') :: xs) name in
      (Instance (name', values) :: xs, has_main, env)
  | [] ->
      ([], has_main, env)

let check ~modul ~interface ~with_main options env x =
  let (res, has_main, env) = from_parse_tree ~current_module:modul ~with_main ~has_main:false options env x in
  if with_main && not has_main then
    Err.fail_module "The 'main' hasn't been found in the main module";
  begin match Env.is_subset_of interface env with
  | [] ->
      ()
  | diff ->
      Err.fail_module
        "The implementation '%s' does not match the interface '%s'.\n\
         Differences: %s"
        (Module.impl modul)
        (Module.intf modul)
        (String.concat ", " diff);
  end;
  res
