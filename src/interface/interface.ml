(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open InterfaceTree

let compile ~current_module options env =
  let rec compile ~env ~local_env = function
    | Val (name, ty) :: xs ->
        let ty = Types.of_parse_tree ~pure_arrow:`Partial options local_env ty in
        let env = Env.add_value name ty env in
        compile ~env ~local_env xs
    | AbstractType (name, k) :: xs ->
        let env = Env.add_type name (Types.Abstract k) env in
        let local_env = Env.add_type name (Types.Abstract k) local_env in
        compile ~env ~local_env xs
    | Datatype (name, k, args, variants) :: xs ->
        let ty_args = List.map fst args in
        let env = Env.add_type name (Types.Abstract k) env in
        let local_env = Env.add_type name (Types.Abstract k) local_env in
        let env =
          let local_env' = List.fold_left (fun local_env (name, k) -> Env.add_type_var name k local_env) local_env args in
          let aux ~datatype env i (name, tys, ty) =
            let tys = List.map (Types.of_parse_tree ~pure_arrow:`Partial options local_env') tys in
            let ty = Types.of_parse_tree ~pure_arrow:`Partial options local_env ty in
            let env = Env.add_variant name (i, ty, List.length tys) env in
            Env.add_constr datatype name ty_args (tys, i) env
          in
          List.foldi (aux ~datatype:name) env variants
        in
        compile ~env ~local_env xs
    | TypeAlias (name, ty) :: xs ->
        let ty = Types.of_parse_tree_kind ~pure_arrow:`Forbid options local_env ty in
        let env = Env.add_type name (Types.Alias ty) env in
        let local_env = Env.add_type name (Types.Alias ty) local_env in
        compile ~env ~local_env xs
    | Exception (name, args) :: xs ->
        let args =
          List.map (Types.of_parse_tree ~pure_arrow:`Forbid options local_env) args
        in
        let env = Env.add_exception name args env in
        let local_env = Env.add_exception name args local_env in
        compile ~env ~local_env xs
    | Class (name, params, sigs) :: xs ->
        let sigs =
          let local_env =
            let aux local_env (name, k) =
              Env.add_type_var name k local_env
            in
            List.fold_left aux local_env params
          in
          let aux (name, ty) =
            (name, Types.of_parse_tree ~pure_arrow:`Forbid options local_env ty)
          in
          List.map aux sigs
        in
        let tyclass = Class.create params sigs in
        let env = Env.add_tyclass name tyclass env in
        let local_env = Env.add_tyclass name tyclass local_env in
        let env =
          let aux env (name_sig, ty) =
            let ty = Types.tyclass_wrap name params ty in
            Env.add_value name_sig ty env
          in
          List.fold_left aux env sigs
        in
        compile ~env ~local_env xs
    | Instance ((tyclass, tys), name) :: xs ->
        let tyclass' = EnvMap.TyClass.find tyclass local_env.Env.tyclasses in
        let tys = List.map (Types.of_parse_tree_kind ~pure_arrow:`Forbid options local_env) tys in
        let (_, tys, tyclass') = Class.add_instance ~tyclass ~current_module tys tyclass' in
        let env = match name with
          | Some name ->
              Env.add_named_instance name (tyclass, tys) env
          | None ->
              env
        in
        let env = Env.replace_tyclass tyclass tyclass' env in
        compile ~env ~local_env xs
    | [] ->
        env
  in
  compile ~env:Env.empty ~local_env:env
