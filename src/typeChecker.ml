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

open TypedTree

let fail_rec_val ~loc_name =
  Err.fail ~loc:loc_name "Recursive values are not allowed"

let rec well_formed_rec = function
  | (_, UnsugaredTree.CAbs _)
  | (_, UnsugaredTree.Abs _) ->
      true
  | (_, UnsugaredTree.TAbs (_, t))
  | (_, UnsugaredTree.Annot (t, _))
  | (_, UnsugaredTree.Let (_, t)) ->
      well_formed_rec t
  | (_, UnsugaredTree.App _)
  | (_, UnsugaredTree.TApp _)
  | (_, UnsugaredTree.CApp _)
  | (_, UnsugaredTree.Val _)
  | (_, UnsugaredTree.PatternMatching _)
  | (_, UnsugaredTree.Fail _)
  | (_, UnsugaredTree.Try _) ->
      false

let rec get_ty_from_let = function
  | (_, UnsugaredTree.Annot (_, ty)) ->
      Some ty
  | (_, UnsugaredTree.Let (_, t)) ->
      get_ty_from_let t
  | (_, UnsugaredTree.CAbs _)
  | (_, UnsugaredTree.Abs _)
  | (_, UnsugaredTree.TAbs _)
  | (_, UnsugaredTree.App _)
  | (_, UnsugaredTree.TApp _)
  | (_, UnsugaredTree.CApp _)
  | (_, UnsugaredTree.Val _)
  | (_, UnsugaredTree.PatternMatching _)
  | (_, UnsugaredTree.Fail _)
  | (_, UnsugaredTree.Try _) ->
      None

let get_rec_ty ~loc_name = function
  | Some (ty, None) ->
      ty
  | Some (_, Some (loc, [])) ->
      Err.fail
        ~loc
        "This empty effect annotation is useless as functions cannot have \
         effects"
  | Some (_, Some (loc, _)) ->
      Err.fail ~loc "Functions doesn't have effects"
  | None ->
      Err.fail
        ~loc:loc_name
        "Recursive functions must have explicit return types"

let get_ty_from_let_rec ~loc_name ty =
  get_rec_ty ~loc_name (get_ty_from_let ty)

let check_type options ~loc_t ~ty:(ty, eff) ~ty_t ~effects gamma =
  let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
  if not (Types.equal ty ty_t) then
    Types.Err.fail ~loc_t ~has:ty_t ~expected:ty;
  begin match eff with
  | Some eff ->
      let eff = Effects.of_list options gamma eff in
      if not (Effects.equal [] eff effects) then
        Err.fail
          ~loc:loc_t
          "This expression has the effect %s but expected the effect %s"
          (Effects.to_string effects)
          (Effects.to_string eff);
  | None ->
      ()
  end

let check_type_opt options ~loc_t ~ty ~ty_t ~effects gamma =
  Option.iter (fun ty -> check_type options ~loc_t ~ty ~ty_t ~effects gamma) ty

let check_effects_forall ~loc_t ~effect =
  (* TODO: Do I need only that to ensure type soundness with side effects ?
     Do I also need to check for exceptions ? *)
  if Effects.has_io effect then
    Err.fail ~loc:loc_t "Cannot have IO effects under a forall"

let rec aux options gamma = function
  | (_, UnsugaredTree.Abs ((name, ty), t)) ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      let gamma = Gamma.add_value name ty gamma in
      let (expr, ty_expr, effect) = aux options gamma t in
      let abs_ty = PrivateTypes.Fun (ty, effect, ty_expr) in
      (Abs (name, expr), abs_ty, Effects.empty)
  | (_, UnsugaredTree.TAbs ((name, k), t)) ->
      let gamma = Gamma.add_type name (Types.Abstract k) gamma in
      let (expr, ty_expr, effect) = aux options gamma t in
      check_effects_forall ~loc_t:(fst t) ~effect;
      let abs_ty = PrivateTypes.Forall (name, k, ty_expr) in
      (expr, abs_ty, effect)
  | (_, UnsugaredTree.CAbs ((name, (tyclass, args)), t)) ->
      let tyclass' = GammaMap.TyClass.find tyclass gamma.Gamma.tyclasses in
      let (gamma, args) =
        let loc = Ident.TyClass.loc tyclass in
        let f = Types.of_parse_tree_kind ~pure_arrow:`Forbid options in
        Class.get_params ~loc f gamma args tyclass'
      in
      let gamma = Gamma.add_named_instance name (tyclass, args) gamma in
      let (expr, ty_expr, effect) = aux options gamma t in
      let abs_ty = PrivateTypes.TyClass ((tyclass, args), effect, ty_expr) in
      (Abs (name, expr), abs_ty, Effects.empty)
  | (loc, UnsugaredTree.App (f, x)) ->
      let loc_f = fst f in
      let loc_x = fst x in
      let (f, ty_f, effect1) = aux options gamma f in
      let (x, ty_x, effect2) = aux options gamma x in
      let (effect3, res, tyclasses) = Types.apply ~loc_f ~loc_x ty_f ty_x in
      let expr =
        let rec aux f n = function
          | (tyclass, args)::xs ->
              begin match Types.get_tys_filled args with
              | [] ->
                  let name =
                    Ident.Name.local_create ~loc:Builtins.unknown_loc "tyclass"
                  in
                  let name = Ident.Name.unique name n in
                  Abs (name, aux (App (f, Val name)) (succ n) xs)
              | tys ->
                  let tyclass = GammaMap.TyClass.find tyclass gamma.Gamma.tyclasses in
                  let name = Class.get_instance_name ~loc tys tyclass in
                  aux (App (f, Val name)) n xs
              end
          | [] ->
              App (f, x)
        in
        aux f 0 tyclasses
      in
      (expr, res, Effects.union3 effect1 effect2 effect3)
  | (_, UnsugaredTree.TApp (f, ty_x)) ->
      let loc_f = fst f in
      let loc_x = fst ty_x in
      let (f, ty_f, effect) = aux options gamma f in
      let (ty_x, kx) = Types.of_parse_tree_kind ~pure_arrow:`Allow options gamma ty_x in
      let (param, res) = Types.apply_ty ~loc_f ~loc_x ~ty_x ~kind_x:kx ty_f in
      let res = Types.replace ~from:param ~ty:ty_x res in
      (f, res, effect)
  | (_, UnsugaredTree.CApp (f, x)) ->
      let (f, ty_f, effect1) = aux options gamma f in
      let (name, tyclass, args) = match x with
        | UnsugaredTree.TyClassVariable name ->
            let (tyclass, args) =
              GammaMap.Instance.find name gamma.Gamma.named_instances
            in
            (name, tyclass, args)
        | UnsugaredTree.TyClassInstance (tyclass, tys) ->
            let aux = Types.of_parse_tree ~pure_arrow:`Allow options gamma in
            let tys = List.map aux tys in
            let tyclass' = GammaMap.TyClass.find tyclass gamma.Gamma.tyclasses in
            let name =
              Class.get_instance_name ~loc:(Ident.TyClass.loc tyclass) tys tyclass'
            in
            (name, tyclass, List.map (fun x -> PrivateTypes.Filled x) tys)
      in
      let (res, effect2) = Types.apply_tyclass ty_f tyclass args in
      (App (f, Val name), res, Effects.union effect1 effect2)
  | (_, UnsugaredTree.Val name) ->
      let (name, ty) = GammaMap.Value.find_binding name gamma.Gamma.values in
      (Val name, ty, Effects.empty)
  | (loc, UnsugaredTree.PatternMatching (t, patterns)) ->
      let loc_t = fst t in
      let (t, ty, effect1) = aux options gamma t in
      if not (Types.is_value ty) then
        Err.fail ~loc:loc_t "This value cannot be matched";
      let (patterns, results, initial_ty, effect2) =
        Pattern.create ~loc (aux options) gamma ty patterns
      in
      let effect = Effects.union effect1 effect2 in
      (PatternMatching (t, results, patterns), initial_ty, effect)
  | (_, UnsugaredTree.Let ((name, UnsugaredTree.NonRec, t), xs)) ->
      let loc_t = fst t in
      let ty = get_ty_from_let t in
      let (t, ty_t, effect1) = aux options gamma t in
      check_type_opt options ~loc_t ~ty ~ty_t ~effects:effect1 gamma;
      let gamma = Gamma.add_value name ty_t gamma in
      let (xs, ty_xs, effect2) = aux options gamma xs in
      (Let (name, t, xs), ty_xs, Effects.union effect1 effect2)
  | (_, UnsugaredTree.Let ((name, UnsugaredTree.Rec, t), xs)) when well_formed_rec t ->
      let ty = get_ty_from_let_rec ~loc_name:(Ident.Name.loc name) t in
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      let gamma = Gamma.add_value name ty gamma in
      let (t, _, effect1) = aux options gamma t in
      let (xs, ty_xs, effect2) = aux options gamma xs in
      (LetRec (name, t, xs), ty_xs, Effects.union effect1 effect2)
  | (_, UnsugaredTree.Let ((name, UnsugaredTree.Rec, _), _)) ->
      fail_rec_val ~loc_name:(Ident.Name.loc name)
  | (loc, UnsugaredTree.Fail (ty, (exn, args))) ->
      let (exn, tys) = GammaMap.Exn.find_binding exn gamma.Gamma.exceptions in
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      let (args, effects) =
        let aux (acc, effects) ty_exn arg =
          let loc_arg = fst arg in
          let (arg, ty_arg, eff) = aux options gamma arg in
          if not (Types.equal ty_arg ty_exn) then
            Types.Err.fail ~loc_t:loc_arg ~has:ty_arg ~expected:ty_exn;
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
  | (_, UnsugaredTree.Try (e, branches)) ->
      let (e, ty, effect) = aux options gamma e in
      let (branches, effect) =
        let aux (branches, effect) ((name, args), t) =
          let (name, tys) = GammaMap.Exn.find_binding name gamma.Gamma.exceptions in
          if Int.Infix.(List.length args <> List.length tys) then
            Err.fail ~loc:(Ident.Exn.loc name) "Wrong number of argument";
          (branches @ [((name, args), t)], Effects.remove_exn name effect)
        in
        List.fold_left aux ([], effect) branches
      in
      let aux (acc, effect) ((name, args), t) =
        let loc_t = fst t in
        let (t, ty', eff) = aux options gamma t in
        if not (Types.equal ty ty') then
          Types.Err.fail ~loc_t ~has:ty' ~expected:ty;
        (((name, args), t) :: acc, Effects.union eff effect)
      in
      let (branches, effect) = List.fold_left aux ([], effect) branches in
      let branches = List.rev branches in
      (Try (e, branches), ty, effect)
  | (_, UnsugaredTree.Annot (t, ty)) ->
      let loc_t = fst t in
      let (_, ty_t, effects) as res = aux options gamma t in
      check_type options ~loc_t ~ty ~ty_t ~effects gamma;
      res

let transform_variants options ~datatype ~ty_args ~args gamma =
  let gamma' = List.fold_left (fun gamma (name, k) -> Gamma.add_type name (Types.Abstract k) gamma) gamma args in
  let rec aux index = function
    | UnsugaredTree.Variant (name, tys, ty) :: xs ->
        let tys = List.map (Types.of_parse_tree ~pure_arrow:`Allow options gamma') tys in
        let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
        let (xs, gamma) = aux (succ index) xs in
        let gamma = Gamma.add_value name ty gamma in
        let gamma = Gamma.add_constr datatype name ty_args (tys, index) gamma in
        (Variant (name, Types.size ty) :: xs, gamma)
    | [] ->
        ([], gamma)
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
      (Ident.Type.to_string (Builtins.t_unit options))
      (Types.to_string ty);
  (is_main, t, ty)

let rec from_parse_tree ~current_module ~with_main ~has_main options gamma = function
  | UnsugaredTree.Value (name, UnsugaredTree.NonRec, term) :: xs ->
      let loc_t = fst term in
      let ty = get_ty_from_let term in
      let (has_main, x, ty_t) = check_effects ~current_module ~with_main ~has_main ~name options (aux options gamma term) in
      check_type_opt options ~loc_t ~ty ~ty_t ~effects:Effects.empty gamma;
      let gamma = Gamma.add_value name ty_t gamma in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      (Value (name, x) :: xs, has_main, gamma)
  | UnsugaredTree.Value (name, UnsugaredTree.Rec, term) :: xs when well_formed_rec term ->
      let ty = get_ty_from_let_rec ~loc_name:(Ident.Name.loc name) term in
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      let gamma = Gamma.add_value name ty gamma in
      let (has_main, x, _) = check_effects ~current_module ~with_main ~has_main ~name options (aux options gamma term) in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      (RecValue (name, x) :: xs, has_main, gamma)
  | UnsugaredTree.Value (name, UnsugaredTree.Rec, _) :: _ ->
      fail_rec_val ~loc_name:(Ident.Name.loc name)
  | UnsugaredTree.Type (name, ty) :: xs ->
      let ty = Types.of_parse_tree_kind ~pure_arrow:`Forbid options gamma ty in
      let gamma = Gamma.add_type name (Types.Alias ty) gamma in
      from_parse_tree ~current_module ~with_main ~has_main options gamma xs
  | UnsugaredTree.Binding (name, ty, binding) :: xs ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      if not (Types.has_io ty) && Types.is_fun ty then
        Err.fail
          ~loc:(Ident.Name.loc name)
          "The binding '%s' cannot be pure. \
           All bindings have to use the IO effect"
          (Ident.Name.to_string name);
      let gamma = Gamma.add_value name ty gamma in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      (Binding (name, Types.size ty, binding) :: xs, has_main, gamma)
  | UnsugaredTree.Datatype (name, kind, args, variants) :: xs ->
      let ty_args = List.map fst args in
      let gamma = Gamma.add_type name (Types.Abstract kind) gamma in
      let (variants, gamma) = transform_variants options ~datatype:name ~ty_args ~args gamma variants in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      (Datatype variants :: xs, has_main, gamma)
  | UnsugaredTree.Exception (name, args) :: xs ->
      let args = List.map (Types.of_parse_tree ~pure_arrow:`Forbid options gamma) args in
      let gamma = Gamma.add_exception name args gamma in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      (Exception name :: xs, has_main, gamma)
  | UnsugaredTree.Open modul :: xs ->
      let gamma = Gamma.open_module modul gamma in
      from_parse_tree ~current_module ~with_main ~has_main options gamma xs
  | UnsugaredTree.Class (name, params, sigs) :: xs ->
      let sigs =
        let gamma =
          let aux gamma (name, k) =
            Gamma.add_type name (Types.Abstract k) gamma
          in
          List.fold_left aux gamma params
        in
        let aux (name, ty) =
          (name, Types.of_parse_tree ~pure_arrow:`Forbid options gamma ty)
        in
        List.map aux sigs
      in
      let tyclass = Class.create params sigs in
      let gamma = Gamma.add_tyclass name tyclass gamma in
      let gamma =
        let aux gamma (name_sig, ty) =
          let ty = Types.tyclass_wrap name params ty in
          Gamma.add_value name_sig ty gamma
        in
        List.fold_left aux gamma sigs
      in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      let (_, xs) =
        let aux (n, xs) (name, _) =
          let abs_name = Ident.Name.create ~loc:Builtins.unknown_loc current_module "0" in
          (succ n, Value (name, Abs (abs_name, RecordGet (Val abs_name, n))) :: xs)
        in
        List.fold_left aux (0, xs) sigs
      in
      (xs, has_main, gamma)
  | UnsugaredTree.Instance (instance, name, values) :: xs ->
      assert false
  | [] ->
      ([], has_main, gamma)

let check ~modul ~interface ~with_main options gamma x =
  let (res, has_main, gamma) = from_parse_tree ~current_module:modul ~with_main ~has_main:false options gamma x in
  if with_main && not has_main then
    Err.fail_module "The 'main' hasn't been found in the main module";
  begin match Gamma.is_subset_of interface gamma with
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
